# Exercises both fail-closed layers from R/build-abr.R and
# R/validate-helpers.R: the dplyr::select(all_of(...)) allowlist and
# the independent check_deny_pattern() regex scan.
test_that("build_abr() selects only ABR_TOURNAMENT_ALLOWLIST columns, dropping unknown upstream keys", {
  tournaments <- tibble::tibble(
    id = "t1", title = "Regional", date = "2023-05-01", format = "standard", type = "regional championship",
    location_state = "NA", location_country = "US",
    location_lat = 51.5, location_lng = -0.12,
    players_count = 32L, top_count = 8L,
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
    id = c("t1", "t2"), title = "Regional", date = "2023-05-01", format = "standard", type = "regional championship",
    location_state = "NA", location_country = "US",
    location_lat = 51.5, location_lng = -0.12,
    players_count = 32L, top_count = 8L,
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
    id = c("t1", "t2"), title = "Regional", date = "2023-05-01", format = "standard", type = "regional championship",
    location_state = "NA", location_country = "US",
    location_lat = 51.5, location_lng = -0.12,
    players_count = 32L, top_count = 8L,
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

# ---- venue coordinates -----------------------------------------------
# Admitted deliberately, reversing part of the DL-002 exclusion. A venue
# is a property of an EVENT, not of a person; every field that identifies
# a person is still excluded, and these tests pin both halves of that.

test_that("venue coordinates reach the processed store", {
  # The point layer of the tournament map cannot be derived from
  # location_country: a country polygon cannot say whether a country's
  # tournaments sit in one city or thirty.
  tournaments <- tibble::tibble(
    id = "t1", title = "Regional", date = "2023-05-01", format = "standard", type = "regional championship",
    location_state = "NA", location_country = "US",
    location_lat = 51.5, location_lng = -0.12,
    players_count = 32L, top_count = 8L,
    winner_runner_identity = "35013", winner_corp_identity = "35068"
  )
  # raw_dir gets its OWN parent: build_abr() writes to
  # dirname(raw_dir)/processed, and withr::local_tempdir() hands every
  # caller a directory inside the one session temp dir -- so two tests
  # sharing it write the same abr.sqlite and the second fails on "table
  # tournament already exists".
  root <- withr::local_tempdir()
  raw_dir <- file.path(root, "raw")
  dir.create(raw_dir)
  staged_raw <- list(tournaments = tournaments, tournament_count = 1L,
                     raw_dir = raw_dir)
  li <- new_lineage("abr", "api_poll", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-abr.R")

  built <- build_abr(li, staged_raw)
  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  withr::defer(DBI::dbDisconnect(con))
  got <- dplyr::collect(dplyr::tbl(con, "tournament"))

  expect_equal(got$location_lat, 51.5)
  expect_equal(got$location_lng, -0.12)
})

test_that("the deny pattern no longer rejects a venue coordinate", {
  # Layer two used to reject location_lat. It must not now, or the
  # allowlist would admit a column the scan then aborts the build over.
  df <- tibble::tibble(id = "t1", location_lat = 51.5, location_lng = -0.12)

  expect_identical(check_deny_pattern(df, ABR_DENY_PATTERN)$status, "pass")
})

test_that("widening for coordinates did not weaken the deny pattern for people", {
  # The whole risk of this change is that it reads as licence to relax
  # the rest. Each of these identifies a person and each must still fail.
  for (col in c("organizer", "creator_name", "player_name", "user_handle",
                "contact", "email", "address", "importer", "uploader")) {
    df <- tibble::tibble(id = "t1")
    df[[col]] <- "x"
    expect_identical(check_deny_pattern(df, ABR_DENY_PATTERN)$status, "fail",
                     info = col)
  }
})

test_that("DEFAULT_DENY_PATTERN still rejects coordinates for every other lineage", {
  # The widening is scoped to abr, where the venue argument applies. A
  # lineage using the default must be unaffected by it.
  df <- tibble::tibble(id = "t1", latitude = 51.5)

  expect_identical(check_deny_pattern(df, DEFAULT_DENY_PATTERN)$status, "fail")
})

test_that("string coordinates from the API are stored as numbers", {
  # The upstream API returns these as strings. Stored as text in a REAL
  # column, every later comparison is lexicographic -- "9.5" sorts above
  # "10.5" -- and a map built on that is wrong with nothing to report it.
  tournaments <- tibble::tibble(
    id = "t1", title = "Regional", date = "2023-05-01", format = "standard", type = "regional championship",
    location_state = "NA", location_country = "US",
    location_lat = "51.5", location_lng = "-0.12",
    players_count = 32L, top_count = 8L,
    winner_runner_identity = "35013", winner_corp_identity = "35068"
  )
  root <- withr::local_tempdir()
  raw_dir <- file.path(root, "raw")
  dir.create(raw_dir)
  staged_raw <- list(tournaments = tournaments, tournament_count = 1L, raw_dir = raw_dir)
  li <- new_lineage("abr", "api_poll", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-abr.R")

  built <- build_abr(li, staged_raw)
  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  withr::defer(DBI::dbDisconnect(con))
  got <- dplyr::collect(dplyr::tbl(con, "tournament"))

  expect_type(got$location_lat, "double")
  expect_equal(got$location_lat, 51.5)
  expect_equal(got$location_lng, -0.12)
})

test_that("an unreadable coordinate becomes NA rather than aborting the build", {
  # A venue with no recorded location is ordinary. The map has no point
  # to plot for it; that is not a reason to fail a release.
  tournaments <- tibble::tibble(
    id = "t1", title = "Regional", date = "2023-05-01", format = "standard", type = "regional championship",
    location_state = "NA", location_country = "US",
    location_lat = "", location_lng = "not a number",
    players_count = 32L, top_count = 8L,
    winner_runner_identity = "35013", winner_corp_identity = "35068"
  )
  root <- withr::local_tempdir()
  raw_dir <- file.path(root, "raw")
  dir.create(raw_dir)
  staged_raw <- list(tournaments = tournaments, tournament_count = 1L, raw_dir = raw_dir)
  li <- new_lineage("abr", "api_poll", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-abr.R")

  expect_no_warning(built <- build_abr(li, staged_raw))
  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  withr::defer(DBI::dbDisconnect(con))
  got <- dplyr::collect(dplyr::tbl(con, "tournament"))

  expect_true(is.na(got$location_lat))
  expect_true(is.na(got$location_lng))
})
