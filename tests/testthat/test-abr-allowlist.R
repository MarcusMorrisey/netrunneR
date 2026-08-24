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

test_that("tournament_id_cardinality passes when permanently-unavailable ids account for the gap", {
  tournaments <- tibble::tibble(
    id = c("t1", "t2"), title = "Regional", date = "2023-05-01", format = "standard",
    location_state = "NA", location_country = "US", players_count = 32L, top_count = 8L,
    winner_runner_identity = "35013", winner_corp_identity = "35068"
  )
  staged_raw <- list(
    tournaments = tournaments, tournament_count = 3L, permanent_ids = "t3",
    raw_dir = file.path(withr::local_tempdir(), "raw")
  )

  li <- new_lineage("abr", "api_poll", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-abr.R")

  built <- build_abr(li, staged_raw)

  id_count_check <- Filter(function(x) identical(x$check, "tournament_id_cardinality"), built$checks)[[1]]
  expect_identical(id_count_check$status, "pass")
})

test_that("tournament_id_cardinality still fails on a genuine mismatch unrelated to permanent_ids", {
  tournaments <- tibble::tibble(
    id = c("t1", "t2"), title = "Regional", date = "2023-05-01", format = "standard",
    location_state = "NA", location_country = "US", players_count = 32L, top_count = 8L,
    winner_runner_identity = "35013", winner_corp_identity = "35068"
  )
  staged_raw <- list(
    tournaments = tournaments, tournament_count = 5L, permanent_ids = "t3",
    raw_dir = file.path(withr::local_tempdir(), "raw")
  )

  li <- new_lineage("abr", "api_poll", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-abr.R")

  built <- build_abr(li, staged_raw)

  id_count_check <- Filter(function(x) identical(x$check, "tournament_id_cardinality"), built$checks)[[1]]
  expect_identical(id_count_check$status, "fail")
})
