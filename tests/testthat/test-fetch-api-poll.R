# Covers the shared netrunneR_api_poll S3 dispatch in R/fetch-api-poll.R,
# not abr-specific pagination detail, which belongs to test-fetch-abr
# fixtures.
test_that("fetch_lineage.netrunneR_api_poll() delegates to the abr helper with tournament_count-derived pagination", {
  # Mirrors the real alwaysberunning.net/api/tournaments/results shape
  # (confirmed live): the response body is a bare JSON array of
  # tournament objects, not a {"results": [...], "tournament_count": N}
  # wrapper, and only the array's first element carries a
  # non-null tournament_count -- every later element's is null.
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
      list(list(id = "t1", title = "Fixture Cup", date = "2023-01-01", tournament_count = 1L)),
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

test_that("fetch_abr() derives tournament_count from the first element only, ignoring later nulls", {
  # A second real-shape page: two rows, only row 1 carries
  # tournament_count -- row 2's is null/NA, same as every non-first row
  # on the live endpoint. Regression coverage for the bug where
  # first_page$tournament_count (the whole column) was compared with
  # stopifnot(tournament_count >= 0), which is NA -- and therefore fails
  # -- as soon as a page has more than one row.
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
      list(
        list(id = "t1", title = "Cup One", date = "2023-01-01", tournament_count = 2L),
        list(id = "t2", title = "Cup Two", date = "2023-01-02", tournament_count = NA)
      ),
      auto_unbox = TRUE, na = "null"
    ))),
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(), auto_unbox = TRUE))), # entries: t1
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(), auto_unbox = TRUE))), # entries: t2
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(), auto_unbox = TRUE))), # videos
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(), auto_unbox = TRUE)))  # upcoming
  ))

  li <- new_lineage("abr", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api")
  attempt_dir <- withr::local_tempdir()

  staged <- fetch_lineage(li, attempt_dir)

  expect_identical(staged$tournament_count, 2L)
  expect_identical(nrow(staged$tournaments), 2L)
})

test_that("a 5xx response is a hard stop", {
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 503, body = charToRaw("{}"))
  ))
  expect_error(abr_get("https://example.test/api", "/tournaments/results"), class = "netrunneR_abr_5xx")
})
