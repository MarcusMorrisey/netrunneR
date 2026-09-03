# Exercises both fail-closed layers from R/build-cobra.R: the
# dplyr::select(all_of(...)) allowlists and the independent
# check_deny_pattern(COBRA_DENY_PATTERN) regex scan, on the same
# precedent as test-abr-allowlist.R.

test_that("check_deny_pattern() with COBRA_DENY_PATTERN flags player display-name column shapes", {
  df <- tibble::tibble(
    tournament_id = "3", player1_id = 501L, player1_name = "Real Name One",
    player_name_with_pronouns = "Real Name One (they/them)"
  )
  result <- check_deny_pattern(df, COBRA_DENY_PATTERN)
  expect_identical(result$status, "fail")
  expect_true(grepl("player1_name", result$message))
})

test_that("check_deny_pattern() with COBRA_DENY_PATTERN flags organizer/contact fields, same as ABR_DENY_PATTERN", {
  df <- tibble::tibble(tournament_id = "3", tournament_organizer = "Alex Organizer", organizer_contact = "x@example.com")
  result <- check_deny_pattern(df, COBRA_DENY_PATTERN)
  expect_identical(result$status, "fail")
})

test_that("check_deny_pattern() with COBRA_DENY_PATTERN passes a table built only from the allowlisted columns", {
  df <- tibble::tibble(
    tournament_id = "3", player1_id = 501L, player1_corp_identity = "Built to Last",
    player1_corp_faction = "haas-bioroid"
  )
  result <- check_deny_pattern(df, COBRA_DENY_PATTERN)
  expect_identical(result$status, "pass")
})

test_that("cobra_bind_allowlisted() errors closed when an allowlist names a missing column", {
  items <- list(tibble::tibble(tournament_id = "3"))
  expect_error(cobra_bind_allowlisted(items, c("tournament_id", "does_not_exist")))
})

test_that("cobra_bind_allowlisted() returns a zero-row, correctly-shaped tibble when every input is empty", {
  out <- cobra_bind_allowlisted(list(NULL, NULL), COBRA_TOURNAMENT_ALLOWLIST)
  expect_identical(nrow(out), 0L)
  expect_setequal(names(out), COBRA_TOURNAMENT_ALLOWLIST)
})

test_that("dplyr::all_of() errors closed when a COBRA allowlist names a missing column", {
  # Same discipline as ABR_TOURNAMENT_ALLOWLIST's equivalent test: a
  # shrunk or mistyped allowlist must fail loudly, not silently select
  # fewer columns than intended.
  df <- tibble::tibble(tournament_id = "3")
  expect_error(dplyr::select(df, dplyr::all_of(COBRA_PAIRING_ALLOWLIST)))
})
