# compute_ice_breaker_matchups() had zero test coverage before this file
# (test-views.R only covers compute_identity_ratings()). These tests
# cover the new matchup_overrides input, credit_differential, and the
# source provenance column added alongside it, using the canonical
# mini-pool fixture (helper-mini-pool.R) rather than inventing a
# per-test sample.

test_that("compute_ice_breaker_matchups() requires matchup_overrides -- no default, no silent skip", {
  expect_error(
    compute_ice_breaker_matchups(mini_pool_ice_breaker_traits(), mini_pool_cardpool()),
    regexp = "matchup_overrides"
  )
})

test_that("subtype-incompatible pairs are still excluded (pre-existing behavior preserved)", {
  result <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )
  m <- result$matchups
  # ice01 (Barrier) x brk02 (Code Gate) shares no subtype token -> must not appear at all.
  expect_equal(nrow(m[m$ice_code == "ice01" & m$breaker_code == "brk02", ]), 0)
})

test_that("override rows win, set source == 'override', and the sign convention matches the docs", {
  result <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )
  m <- result$matchups

  runner_row <- m[m$ice_code == PAIR_RUNNER_FAVORED$ice_code & m$breaker_code == PAIR_RUNNER_FAVORED$breaker_code, ]
  expect_equal(runner_row$source, "override")
  expect_equal(runner_row$cost_to_break, 1L)
  expect_equal(runner_row$credit_differential, 1L)   # 2 (rez) - 1 (break) = +1, favors runner
  expect_gt(runner_row$credit_differential, 0)

  corp_row <- m[m$ice_code == PAIR_CORP_FAVORED$ice_code & m$breaker_code == PAIR_CORP_FAVORED$breaker_code, ]
  expect_equal(corp_row$source, "override")
  expect_equal(corp_row$cost_to_break, 10L)
  expect_equal(corp_row$credit_differential, -2L)    # 8 (rez) - 10 (break) = -2, favors corp
  expect_lt(corp_row$credit_differential, 0)
})

test_that("a subtype-compatible pair with no override is honestly not_computable, never a fabricated number", {
  result <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )
  m <- result$matchups
  row <- m[m$ice_code == "ice03" & m$breaker_code == "brk03", ]

  expect_equal(nrow(row), 1)
  expect_equal(row$source, "not_computable")
  expect_true(is.na(row$cost_to_break))
  expect_true(is.na(row$credit_differential))
})

test_that("cost_to_break and credit_differential agree on NA-ness in every row", {
  result <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )
  m <- result$matchups
  expect_equal(is.na(m$cost_to_break), is.na(m$credit_differential))
  expect_equal(is.na(m$cost_to_break), m$source == "not_computable")
})

test_that("manifest cache_identity changes with cardpool/implementation release id and overrides content", {
  overrides <- mini_pool_matchup_overrides()

  base <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), overrides,
    cardpool_release_id = "cp1", implementation_release_id = "impl1"
  )
  diff_cardpool_id <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), overrides,
    cardpool_release_id = "cp2", implementation_release_id = "impl1"
  )
  diff_overrides <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), overrides[1, ],
    cardpool_release_id = "cp1", implementation_release_id = "impl1"
  )

  ids <- c(base$manifest$cache_identity, diff_cardpool_id$manifest$cache_identity, diff_overrides$manifest$cache_identity)
  expect_equal(length(unique(ids)), 3)
})
