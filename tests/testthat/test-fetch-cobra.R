# Covers R/fetch-cobra.R: dispatch, cobra_get()'s status handling and
# pacing, the discovery/tail-probe/backfill-walk crawl's pure helpers,
# and the checkpointed object pool's content identity.

test_that("fetch_lineage.netrunneR_api_poll() dispatches lineage 'cobra' to fetch_cobra()", {
  called_with <- NULL
  testthat::local_mocked_bindings(
    fetch_cobra = function(lineage, attempt_dir) {
      called_with <<- list(lineage = lineage, attempt_dir = attempt_dir)
      list(ok = TRUE)
    }
  )
  li <- new_lineage("cobra", "api_poll", withr::local_tempdir(),
                    base_url = "https://example.test", pacing = list(min_delay_s = 1, max_delay_s = 2))
  attempt_dir <- withr::local_tempdir()

  result <- fetch_lineage(li, attempt_dir)

  expect_identical(result$ok, TRUE)
  expect_identical(called_with$lineage$name, "cobra")
  expect_identical(called_with$attempt_dir, attempt_dir)
})

test_that("cobra_get() treats 301/302/303/307/308/401/403/404/406 as 'does not exist', not an error", {
  # 302 is the live behavior for a missing tournament id (confirmed against
  # /tournaments/99999/rounds/pairings_data, 2026-09-04): the site redirects
  # to /error rather than 404ing. The other 3xx codes are included on the
  # same reasoning, not because they are individually confirmed live -- any
  # redirect away from the requested resource means "not this", the same
  # semantic 401/403/404/406 already cover.
  li <- list(base_url = "https://example.test", pacing = list(min_delay_s = 1, max_delay_s = 2))
  for (code in c(301L, 302L, 303L, 307L, 308L, 401L, 403L, 404L, 406L)) {
    httr2::local_mocked_responses(list(httr2::response(status_code = code, body = charToRaw("{}"))))
    result <- cobra_get(li, "/tournaments/999999/rounds/pairings_data")
    expect_false(result$ok, info = as.character(code))
    expect_identical(result$status, code, info = as.character(code))
    expect_null(result$body, info = as.character(code))
  }
})

test_that("cobra_get() returns the parsed body with simplifyVector = FALSE on 2xx", {
  li <- list(base_url = "https://example.test", pacing = list(min_delay_s = 1, max_delay_s = 2))
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, body = charToRaw('{"tournament":{"id":1,"name":"Cup"}}'))
  ))
  result <- cobra_get(li, "/tournaments/1/rounds/pairings_data")
  expect_true(result$ok)
  expect_identical(result$status, 200L)
  # simplifyVector = FALSE keeps a scalar as a length-1 list, not an
  # atomic vector -- the shape flatten_cobra_*() (R/build-cobra.R) walks
  # with $ access.
  expect_identical(result$body$tournament$name[[1]], "Cup")
})

test_that("cobra_get() retries a 5xx/429 with backoff and eventually hard-stops", {
  li <- list(base_url = "https://example.test", pacing = list(min_delay_s = 1, max_delay_s = 2))
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 503, body = charToRaw("{}")),
    httr2::response(status_code = 503, body = charToRaw("{}")),
    httr2::response(status_code = 503, body = charToRaw("{}")),
    httr2::response(status_code = 503, body = charToRaw("{}"))
  ))
  expect_error(
    cobra_get(li, "/tournaments/1/rounds/pairings_data", max_retries = 4L),
    class = "netrunneR_cobra_5xx"
  )
})

test_that("cobra_get() derives its req_throttle() rate from lineage$pacing via pacing_rate()", {
  captured_pacing <- NULL
  testthat::local_mocked_bindings(
    pacing_rate = function(pacing) {
      captured_pacing <<- pacing
      1 / 2
    }
  )
  httr2::local_mocked_responses(list(httr2::response(status_code = 200, body = charToRaw("{}"))))

  custom_pacing <- list(min_delay_s = 5, max_delay_s = 9)
  li <- list(base_url = "https://example.test", pacing = custom_pacing)
  cobra_get(li, "/tournaments/1/rounds/pairings_data")

  expect_identical(captured_pacing, custom_pacing)
})

test_that("scrape_cobra_bundle() reports exists = FALSE when pairings itself is missing", {
  testthat::local_mocked_bindings(
    cobra_get = function(lineage, path, ...) list(ok = FALSE, status = 404L, body = NULL)
  )
  bundle <- scrape_cobra_bundle(list(), 42L)
  expect_false(bundle$exists)
  expect_identical(bundle$tournament_id, 42L)
})

test_that("scrape_cobra_bundle() treats a missing companion endpoint as NULL, not a failure", {
  testthat::local_mocked_bindings(
    cobra_get = function(lineage, path, ...) {
      if (grepl("pairings_data", path, fixed = TRUE)) {
        list(ok = TRUE, status = 200L, body = list(tournament = list(id = 42L)))
      } else {
        list(ok = FALSE, status = 404L, body = NULL)
      }
    }
  )
  bundle <- scrape_cobra_bundle(list(), 42L)
  expect_true(bundle$exists)
  expect_identical(bundle$pairings_data$tournament$id, 42L)
  expect_null(bundle$standings_data)
  expect_null(bundle$id_and_faction_data)
  expect_null(bundle$cut_conversion_rates)
})

test_that("record_cobra_bundle() writes a resolved bundle to the pool and upserts checkpoint", {
  pool_dir <- withr::local_tempdir()
  checkpoint <- tibble::tibble(tournament_id = character(0), exists = logical(0), last_fetched_at = character(0))
  bundle <- list(exists = TRUE, tournament_id = 7L, pairings_data = list(tournament = list(id = 7L)),
                 standings_data = NULL, id_and_faction_data = NULL, cut_conversion_rates = NULL)

  checkpoint <- record_cobra_bundle(pool_dir, checkpoint, bundle)

  expect_true(fs::file_exists(file.path(pool_dir, "7.json")))
  expect_true(checkpoint$exists[checkpoint$tournament_id == "7"])
})

test_that("record_cobra_bundle() settles a non-existent id without writing a pool object", {
  pool_dir <- withr::local_tempdir()
  checkpoint <- tibble::tibble(tournament_id = character(0), exists = logical(0), last_fetched_at = character(0))
  bundle <- list(exists = FALSE, tournament_id = 8L)

  checkpoint <- record_cobra_bundle(pool_dir, checkpoint, bundle)

  expect_false(fs::file_exists(file.path(pool_dir, "8.json")))
  expect_false(checkpoint$exists[checkpoint$tournament_id == "8"])
})

test_that("advance_cobra_backfill() never re-attempts an id already checkpointed", {
  pool_dir <- withr::local_tempdir()
  checkpoint <- tibble::tibble(tournament_id = "1", exists = TRUE, last_fetched_at = "2026-01-01T00:00:00Z")
  crawl_state <- list(max_known_id = 2L, backfill_next_id = 1L, backfill_complete = FALSE)

  attempted <- integer(0)
  testthat::local_mocked_bindings(
    scrape_cobra_bundle = function(lineage, tournament_id) {
      attempted <<- c(attempted, tournament_id)
      list(exists = TRUE, tournament_id = tournament_id, pairings_data = list(tournament = list(id = tournament_id)),
           standings_data = NULL, id_and_faction_data = NULL, cut_conversion_rates = NULL)
    }
  )

  result <- advance_cobra_backfill(list(), checkpoint, crawl_state, pool_dir)

  expect_identical(attempted, 2L)
  expect_true(result$crawl_state$backfill_complete)
})

test_that("advance_cobra_backfill() marks backfill_complete once the walk passes max_known_id", {
  crawl_state <- list(max_known_id = 5L, backfill_next_id = 6L, backfill_complete = FALSE)
  result <- advance_cobra_backfill(list(), tibble::tibble(tournament_id = character(0), exists = logical(0), last_fetched_at = character(0)), crawl_state, withr::local_tempdir())
  expect_true(result$crawl_state$backfill_complete)
})

test_that("cobra_content_identity() changes when an object file's content changes, not just when ids change", {
  pool_dir <- withr::local_tempdir()
  jsonlite::write_json(list(a = 1), file.path(pool_dir, "1.json"), auto_unbox = TRUE)

  before <- cobra_content_identity(pool_dir, "1")
  jsonlite::write_json(list(a = 2), file.path(pool_dir, "1.json"), auto_unbox = TRUE)
  after <- cobra_content_identity(pool_dir, "1")

  expect_false(identical(before, after))
})

test_that("cobra_content_identity() is stable under a re-fetch that changes nothing", {
  pool_dir <- withr::local_tempdir()
  jsonlite::write_json(list(a = 1), file.path(pool_dir, "1.json"), auto_unbox = TRUE)

  first <- cobra_content_identity(pool_dir, "1")
  second <- cobra_content_identity(pool_dir, "1")

  expect_identical(first, second)
})

test_that("check_cobra_pool_nonempty() warns rather than fails on an empty pool", {
  expect_identical(check_cobra_pool_nonempty(list())$status, "warn")
  expect_identical(check_cobra_pool_nonempty(list(a = list()))$status, "pass")
})

test_that("check_cobra_bundle_shape() fails when a bundle is missing pairings_data$tournament", {
  good <- list(pairings_data = list(tournament = list(id = 1L)))
  bad <- list(pairings_data = list())
  expect_identical(check_cobra_bundle_shape(list(good))$status, "pass")
  expect_identical(check_cobra_bundle_shape(list(bad))$status, "fail")
})
