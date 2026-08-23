# Covers the shared netrunneR_api_poll S3 dispatch in R/fetch-api-poll.R,
# not abr-specific pagination detail, which belongs to test-fetch-abr
# fixtures.
test_that("fetch_lineage.netrunneR_api_poll() delegates to the abr helper with tournament_count-derived pagination", {
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
      list(tournament_count = 1L, results = list(list(id = "t1", title = "Fixture Cup", date = "2023-01-01"))),
      auto_unbox = TRUE
    ))),
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(), auto_unbox = TRUE))),
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(), auto_unbox = TRUE))),
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(), auto_unbox = TRUE)))
  ))

  li <- new_lineage("abr", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api")
  attempt_dir <- withr::local_tempdir()

  staged <- fetch_lineage(li, attempt_dir)

  expect_identical(staged$tournament_count, 1L)
  expect_identical(nrow(staged$tournaments), 1L)
})

test_that("a 5xx response is a hard stop", {
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 503, body = charToRaw("{}"))
  ))
  expect_error(abr_get("https://example.test/api", "/tournaments/results"), class = "netrunneR_abr_5xx")
})
