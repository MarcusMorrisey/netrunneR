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

test_that("fetch_lineage.netrunneR_api_poll() resumes ABR entries from a pre-existing checkpoint, skipping already-resolved tournaments", {
  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api")

  # Pre-seed the pool/checkpoint as if a prior, interrupted attempt had
  # already resolved t1's entries -- fetch_abr() now routes the entries
  # step through run_abr_backfill()'s checkpoint (R/abr-backfill.R), so
  # this must be honored on the very next fetch_lineage() call, not just
  # on a direct run_abr_backfill() re-run.
  pool_dir <- file.path(store_root, "objects")
  fs::dir_create(pool_dir, mode = "0700")
  jsonlite::write_json(list(note = "seeded-t1"), file.path(pool_dir, "t1.json"), auto_unbox = TRUE)
  saveRDS(
    tibble::tibble(tournament_id = "t1", resolved = TRUE),
    file.path(store_root, "backfill-checkpoint.rds")
  )

  # Only t2's entries endpoint should be requested live. If t1 were
  # refetched instead of read from checkpoint, this response would be
  # consumed by t1's request and every request after it would be off by
  # one -- t2 would get the videos mock, and the upcoming request would
  # run out of mocked responses and error loudly rather than silently
  # returning the wrong content.
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
      list(
        list(id = "t1", title = "Cup One", date = "2023-01-01", tournament_count = 2L),
        list(id = "t2", title = "Cup Two", date = "2023-01-02", tournament_count = NA)
      ),
      auto_unbox = TRUE, na = "null"
    ))),
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(note = "live-t2"), auto_unbox = TRUE))), # entries: t2 only
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(), auto_unbox = TRUE))), # videos
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(), auto_unbox = TRUE)))  # upcoming
  ))

  attempt_dir <- withr::local_tempdir()
  staged <- fetch_lineage(li, attempt_dir)

  expect_identical(staged$entries[[1]]$note, "seeded-t1")
  expect_identical(staged$entries[[2]]$note, "live-t2")
})

test_that("a 5xx response is a hard stop", {
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 503, body = charToRaw("{}"))
  ))
  expect_error(abr_get("https://example.test/api", "/tournaments/results"), class = "netrunneR_abr_5xx")
})
