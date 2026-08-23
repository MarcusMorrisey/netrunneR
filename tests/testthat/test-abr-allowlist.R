# Exercises both fail-closed layers from R/build-abr.R and
# R/validate-helpers.R: the dplyr::select(all_of(...)) allowlist and
# the independent check_deny_pattern() regex scan.
test_that("build_abr() selects only ABR_TOURNAMENT_ALLOWLIST columns, dropping unknown upstream keys", {
  tournaments <- tibble::tibble(
    id = "t1", title = "Regional", date = "2023-05-01", format = "standard",
    location_state = "NA", location_country = "US", players_count = 32L, top_count = 8L,
    winner_runner_identity = "35013", winner_corp_identity = "35068",
    organizer_email = "leak@example.com"
  )
  staged_raw <- list(tournaments = tournaments, tournament_count = 1L, raw_dir = withr::local_tempdir())

  li <- new_lineage("abr", "api_poll", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-abr.R")

  built <- build_abr(li, staged_raw)

  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  withr::defer(DBI::dbDisconnect(con))
  expect_setequal(names(dplyr::tbl(con, "tournament") |> dplyr::collect()), ABR_TOURNAMENT_ALLOWLIST)
})

test_that("dplyr::all_of() errors closed when ABR_TOURNAMENT_ALLOWLIST names a missing column", {
  tournaments <- tibble::tibble(id = "t1")
  expect_error(dplyr::select(tournaments, dplyr::all_of(ABR_TOURNAMENT_ALLOWLIST)))
})

test_that("check_deny_pattern() with ABR_DENY_PATTERN flags a personal-data column name", {
  df <- tibble::tibble(id = "t1", organizer_email = "x@example.com")
  result <- check_deny_pattern(df, ABR_DENY_PATTERN)
  expect_identical(result$status, "fail")
})
