build_mini_matchup <- function() {
  compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )$matchups
}

test_that("matchup_display() with a NULL code set returns every row sorted by cost_to_break", {
  matchup <- build_mini_matchup()
  cards <- mini_pool_cardpool()

  result <- matchup_display(matchup, NULL, cards)

  expect_equal(nrow(result), nrow(matchup))
  expect_true(all(c("ice_title", "breaker_title") %in% names(result)))
  ordered_costs <- result$cost_to_break
  non_na <- ordered_costs[!is.na(ordered_costs)]
  expect_equal(non_na, sort(non_na))
  # NA rows sort last.
  na_positions <- which(is.na(ordered_costs))
  non_na_positions <- which(!is.na(ordered_costs))
  if (length(na_positions) > 0 && length(non_na_positions) > 0) {
    expect_true(min(na_positions) > max(non_na_positions))
  }
})

test_that("matchup_display() with a code set drops a pair whose breaker is outside the set", {
  matchup <- build_mini_matchup()
  cards <- mini_pool_cardpool()

  # ice01 pairs with brk01 among others; excluding brk01 from the code set
  # should drop that pair while keeping ice01 paired with a permitted breaker.
  codes <- setdiff(unique(c(matchup$ice_code, matchup$breaker_code)), "brk01")

  result <- matchup_display(matchup, codes, cards)

  expect_false(any(result$breaker_code == "brk01"))
  expect_true(all(result$ice_code %in% codes))
  expect_true(all(result$breaker_code %in% codes))
})

test_that("matchup_na_dash() returns the em dash for NA and the value otherwise", {
  expect_equal(matchup_na_dash(NA_real_), "\u2014")
  expect_equal(matchup_na_dash(5), 5)
  expect_equal(matchup_na_dash("not_computable"), "not_computable")
})
