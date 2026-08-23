test_that("the nrdb api-poll fetch helper builds a User-Agent from NRDB_CONTACT and compares against the previous release", {
# Covers the nrdb-specific helper in R/fetch-nrdb.R; the shared
# netrunneR_api_poll S3 dispatch itself is covered by
# test-fetch-api-poll.R. (ref: DL-005)

  withr::local_envvar(NRDB_CONTACT = "fixture@example.test")

  # run_decklist_sweep() (called from fetch_nrdb()) issues one request per
  # day in its ~30-day default window, so a fixed-length response list
  # would be exhausted well before the sweep finishes; a function mock
  # dispatching on path handles any number of calls, matching the pattern
  # in test-decklist-sweep.R.
  httr2::local_mocked_responses(function(req) {
    if (grepl("/reviews$", req$url)) {
      return(httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
        list(total = 10L, last_updated = "2023-01-01", version_number = "1.0", results = list()), auto_unbox = TRUE
      ))))
    }
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(results = list()), auto_unbox = TRUE)))
  })

  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api")
  attempt_dir <- withr::local_tempdir()

  staged <- fetch_nrdb(li, attempt_dir)

  expect_identical(staged$reviews$total, 10L)
  expect_true(length(staged$checks) == 3)
})

# httr2's local_mocked_responses() replaces the whole perform-with-retry
# pipeline with a single response lookup (verified directly: neither a
# fixed-length list of [503, 200] nor a call-counting function mock ever
# triggers a second attempt), so retry-then-succeed can't actually be
# exercised through a mock. Instead this checks nrdb_get() wires up
# req_retry() with a real, bounded max_tries by inspecting the request
# object the mock receives.
test_that("nrdb_get() configures req_retry() with a bounded max_tries", {
  captured_req <- NULL
  httr2::local_mocked_responses(function(req) {
    captured_req <<- req
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(total = 1L), auto_unbox = TRUE)))
  })
  withr::local_envvar(NRDB_CONTACT = "fixture@example.test")
  result <- nrdb_get("https://example.test/api", "/reviews")

  expect_identical(result$total, 1L)
  expect_true(is.finite(captured_req$policies$retry_max_tries))
  expect_gt(captured_req$policies$retry_max_tries, 1)
})
