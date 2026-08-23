# Tests build_nrdb() against the real /reviews and /rulings field shapes
# (confirmed live against netrunnerdb.com/api/2.0/public): the schema's
# original column names (card_code, text, created_at) never existed
# upstream, matching the same root mistake the now-removed decklist
# table had. raw_dir is nested one level under a fresh attempt
# directory, matching what fetch_nrdb() actually produces in production
# (db_path is derived from dirname(raw_dir)). Fixtures are plain tibbles
# (not nested lists) to match what jsonlite::fromJSON(simplifyVector =
# TRUE) actually produces for an array of objects.
test_that("build_nrdb() writes review/ruling using the real upstream field shapes", {
  raw_dir <- file.path(withr::local_tempdir(), "raw")
  fs::dir_create(raw_dir)

  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api",
                    schema_version = 1L, build_module_path = "R/build-nrdb.R")

  staged_raw <- list(
    raw_dir = raw_dir,
    reviews = list(data = tibble::tibble(
      id = 2L, title = "Mushin No Shin", user = "Ber", ruling = "Pros and cons.",
      votes = 5L, date_create = "2017-01-01", date_update = "2017-01-02"
    )),
    rulings = list(data = tibble::tibble(
      # /rulings carries no id field at all -- confirmed live.
      title = "Djinn", ruling = "Cannot move programs onto Djinn later.",
      date_update = "2017-04-08", nsg_rules_team_verified = FALSE
    )),
    checks = list()
  )

  built <- build_nrdb(li, staged_raw)

  expect_true(fs::file_exists(built$db_path))
  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  on.exit(DBI::dbDisconnect(con))

  reviews <- DBI::dbReadTable(con, "review")
  expect_identical(reviews$title, "Mushin No Shin")
  expect_identical(reviews$user, "Ber")

  rulings <- DBI::dbReadTable(con, "ruling")
  expect_identical(rulings$title, "Djinn")
})

test_that("build_nrdb() drops unrecognized upstream keys instead of erroring", {
  # The real /reviews envelope carries no fields beyond
  # NRDB_REVIEW_ALLOWLIST today, but a future upstream addition must be
  # dropped, not crash dbWriteTable() the way the pre-fix schema did.
  raw_dir <- file.path(withr::local_tempdir(), "raw")
  fs::dir_create(raw_dir)

  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api",
                    schema_version = 1L, build_module_path = "R/build-nrdb.R")

  staged_raw <- list(
    raw_dir = raw_dir,
    reviews = list(data = tibble::tibble(
      id = 1L, title = "t", user = "u", ruling = "r", votes = 0L,
      date_create = "2017-01-01", date_update = "2017-01-01", future_field = "unexpected"
    )),
    rulings = list(data = tibble::tibble(
      title = "t", ruling = "r", date_update = "2017-01-01", nsg_rules_team_verified = TRUE
    )),
    checks = list()
  )

  built <- build_nrdb(li, staged_raw)
  expect_true(fs::file_exists(built$db_path))
})

# Regression test for a real crash: /reviews' `comments` field is not a
# count but a nested array of comment-thread objects (user, comment,
# date_create, date_update) per review, confirmed live. jsonlite parses
# it as a list-column, which DBI::dbWriteTable() cannot bind ("Can only
# bind lists of raw vectors (or NULL)") -- comments must be dropped by
# the allowlist select() before it ever reaches dbWriteTable().
test_that("build_nrdb() drops the nested comments list-column instead of crashing dbWriteTable()", {
  raw_dir <- file.path(withr::local_tempdir(), "raw")
  fs::dir_create(raw_dir)

  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api",
                    schema_version = 1L, build_module_path = "R/build-nrdb.R")

  reviews <- tibble::tibble(
    id = 2L, title = "Mushin No Shin", user = "Ber", ruling = "Pros and cons.",
    votes = 5L, date_create = "2017-01-01", date_update = "2017-01-02"
  )
  reviews$comments <- list(tibble::tibble(
    user = "ECAbrams", comment = "nested thread", date_create = "2018-01-01", date_update = "2018-01-01"
  ))

  staged_raw <- list(
    raw_dir = raw_dir,
    reviews = list(data = reviews),
    rulings = list(data = tibble::tibble(
      title = "Djinn", ruling = "Cannot move programs onto Djinn later.",
      date_update = "2017-04-08", nsg_rules_team_verified = FALSE
    )),
    checks = list()
  )

  built <- build_nrdb(li, staged_raw)
  expect_true(fs::file_exists(built$db_path))
})
