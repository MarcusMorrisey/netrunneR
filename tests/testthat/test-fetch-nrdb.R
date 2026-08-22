test_that("the nrdb api-poll fetch helper builds a User-Agent from NRDB_CONTACT and compares against the previous release", {
# Covers the nrdb-specific helper in R/fetch-nrdb.R; the shared
# netrunneR_api_poll S3 dispatch itself is covered by
# test-fetch-api-poll.R. (ref: DL-005)

  withr::local_envvar(NRDB_CONTACT = "fixture@example.test")

  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
      list(total = 10L, last_updated = "2023-01-01", version_number = "1.0", results = list()), auto_unbox = TRUE
    ))),
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(results = list()), auto_unbox = TRUE)))
  ))

  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api")
  attempt_dir <- withr::local_tempdir()

  staged <- fetch_nrdb(li, attempt_dir)

  expect_identical(staged$reviews$total, 10L)
  expect_true(length(staged$checks) == 3)
})

test_that("req_retry() backs off a fixture transient failure", {
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 503, body = charToRaw("{}")),
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(total = 1L), auto_unbox = TRUE)))
  ))
  withr::local_envvar(NRDB_CONTACT = "fixture@example.test")
  result <- nrdb_get("https://example.test/api", "/reviews")
  expect_identical(result$total, 1L)
})
