test_that("resolve_deck_codes() puts an unknown code in unresolved and still resolves the rest", {
  all_codes <- mini_pool_cardpool()[c("code", "title", "type_code")]
  deck <- list(
    id = 1, name = "Test Corp", identity_code = "id01",
    cards = c(ice01 = 3L, zzz99 = 2L)
  )

  resolved <- resolve_deck_codes(deck, all_codes)

  expect_equal(resolved$unresolved, "zzz99")
  expect_equal(resolved$ice, "ice01")
})

test_that("resolve_deck_codes() puts an agenda code in other_known, not unresolved", {
  # mini_pool_cardpool() already includes an agenda row (agn01): present in
  # all_codes but neither ice nor a breaker, so it must land in other_known,
  # not unresolved.
  all_codes <- mini_pool_cardpool()[c("code", "title", "type_code")]
  deck <- list(
    id = 1, name = "Test Corp", identity_code = "id01",
    cards = c(ice01 = 3L, agn01 = 3L)
  )

  resolved <- resolve_deck_codes(deck, all_codes)

  expect_equal(resolved$other_known, "agn01")
  expect_false("agn01" %in% resolved$unresolved)
})

test_that("resolve_deck_codes() refuses a zero-card deck", {
  all_codes <- mini_pool_cardpool()[c("code", "title", "type_code")]
  deck <- list(id = 1, name = "Empty", identity_code = "id01", cards = c())

  expect_error(resolve_deck_codes(deck, all_codes))
})

test_that("resolve_deck_codes() refuses a deck with no identity_code", {
  all_codes <- mini_pool_cardpool()[c("code", "title", "type_code")]
  deck <- list(id = 1, name = "No Identity", identity_code = NA_character_, cards = c(ice01 = 3L))

  expect_error(resolve_deck_codes(deck, all_codes))
})

test_that("deck_matchups() returns one row per (deck ice title) x (deck breaker title) present in the matchup tibble", {
  matchup <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )$matchups

  # All four breakers are "Icebreaker - Fracter", matching ice01's "Barrier"
  # subtype, so every (ice, breaker) combination is present in the matchup
  # tibble -- the row count equals the product of unique titles on each side.
  corp_ice_codes <- c("ice01")
  runner_breaker_codes <- c("brk01", "brk04", "brk05", "brk06")
  corp_copy_counts <- c(ice01 = 3L)

  result <- deck_matchups(matchup, corp_ice_codes, runner_breaker_codes, corp_copy_counts)

  expected_rows <- matchup[
    matchup$ice_code %in% corp_ice_codes & matchup$breaker_code %in% runner_breaker_codes,
  ]

  expect_equal(nrow(result$matchups), nrow(expected_rows))
  expect_equal(
    nrow(result$matchups),
    length(unique(corp_ice_codes)) * length(unique(runner_breaker_codes))
  )
})

test_that("deck_matchups() returns exactly compute_ice_breaker_matchups()'s columns", {
  matchup <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )$matchups

  result <- deck_matchups(matchup, c("ice01"), c("brk01"), c(ice01 = 3L))

  expect_equal(names(result$matchups), names(matchup))
})

test_that("deck_matchups() returns copy counts as a separate lookup, not a column", {
  matchup <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )$matchups

  result <- deck_matchups(matchup, c("ice01"), c("brk01"), c(ice01 = 3L))

  expect_false("copy_count" %in% names(result$matchups))
  expect_equal(unname(result$copy_counts["ice01"]), 3L)
})
