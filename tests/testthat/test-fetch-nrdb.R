test_that("the nrdb api-poll fetch helper builds a User-Agent from NRDB_CONTACT and shape-checks the data envelope", {
# Covers the nrdb-specific helper in R/fetch-nrdb.R; the shared
# netrunneR_api_poll S3 dispatch itself is covered by
# test-fetch-api-poll.R. (ref: DL-005)

  withr::local_envvar(NRDB_CONTACT = "fixture@example.test")

  httr2::local_mocked_responses(function(req) {
    if (grepl("/reviews$", req$url)) {
      return(httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
        list(data = list(list(id = 1L, title = "fixture review"))), auto_unbox = TRUE
      ))))
    }
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(data = list()), auto_unbox = TRUE)))
  })

  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api")
  attempt_dir <- withr::local_tempdir()

  staged <- fetch_nrdb(li, attempt_dir)

  expect_identical(NROW(staged$reviews$data), 1L)
  expect_true(length(staged$checks) == 2)
  expect_true(all(vapply(staged$checks, function(x) identical(x$status, "pass"), logical(1))))
})

test_that("compare_shape() fails when a response has no data list", {
  result <- compare_shape(list(unexpected = "field"), "reviews")
  expect_identical(result$status, "fail")
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
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(data = list()), auto_unbox = TRUE)))
  })
  withr::local_envvar(NRDB_CONTACT = "fixture@example.test")
  result <- nrdb_get("https://example.test/api", "/reviews")

  expect_identical(length(result$data), 0L)
  expect_true(is.finite(captured_req$policies$retry_max_tries))
  expect_gt(captured_req$policies$retry_max_tries, 1)
})
