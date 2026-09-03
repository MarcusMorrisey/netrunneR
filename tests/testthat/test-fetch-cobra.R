# Covers R/fetch-cobra.R: type-page discovery, new-id tail probing, the
# checkpointed object pool, and the bounded historical backfill walk.
# Every test lineage uses a tiny pacing range (min/max ~0.01s) so the
# ~40-50 mocked requests these tests issue (12 type pages + a fixed
# COBRA_TAIL_MISS_LIMIT-sized tail probe + a small backfill batch) do
# not incur the multi-second req_throttle() delays a production pacing
# value would add per request.

cobra_fixture_lineage <- function(store_root, base_url = "https://example.test") {
  new_lineage(
    "cobra", "api_poll", store_root,
    base_url = base_url, schema_version = 1L, build_module_path = "R/build-cobra.R",
    pacing = list(min_delay_s = 0.01, max_delay_s = 0.01)
  )
}

# A single tournament (id 3) is discoverable via type page 1; every other
# type page 404s, every other tournament id 404s on pairings_data (so the
# tail probe above id 3 and the low-end backfill walk both settle on
# "not found" quickly).
cobra_single_tournament_mock <- function() {
  function(req) {
    url <- req$url
    if (grepl("/tournaments/type/1$", url)) {
      return(httr2::response(status_code = 200, body = charToRaw('<a href="/tournaments/3">Fixture Cup</a>')))
    }
    if (grepl("/tournaments/type/[0-9]+$", url)) {
      return(httr2::response(status_code = 404))
    }
    if (grepl("/tournaments/3/rounds/pairings_data$", url)) {
      return(httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
        list(
          tournament = list(id = 3, name = "Fixture Cup", slug = "fixture-cup", date = "2024-01-01"),
          stages = list(),
          policy = list(update = FALSE, custom_table_numbering = FALSE)
        ),
        auto_unbox = TRUE
      ))))
    }
    if (grepl("/tournaments/3/players/standings_data$", url)) {
      return(httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
        list(tournament_id = 3, stages = list()), auto_unbox = TRUE
      ))))
    }
    if (grepl("/tournaments/3/(id_and_faction_data|cut_conversion_rates)$", url)) {
      return(httr2::response(status_code = 404))
    }
    # Any other tournament id's pairings_data (tail probe misses, backfill
    # walk misses on ids 1 and 2).
    httr2::response(status_code = 404)
  }
}

test_that("cobra_get() treats a 404 as ok = FALSE rather than an error", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 404))
  li <- list(base_url = "https://example.test", pacing = list(min_delay_s = 0.01, max_delay_s = 0.01))
  result <- cobra_get(li, "/tournaments/999/rounds/pairings_data")
  expect_false(result$ok)
  expect_identical(result$status, 404L)
})

test_that("cobra_get() parses a 200 JSON body and derives its throttle rate from lineage$pacing", {
  captured_pacing <- NULL
  testthat::local_mocked_bindings(
    pacing_rate = function(pacing) {
      captured_pacing <<- pacing
      1 / 0.01
    }
  )
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(ok = TRUE), auto_unbox = TRUE)))
  })
  custom_pacing <- list(min_delay_s = 3, max_delay_s = 7)
  li <- list(base_url = "https://example.test", pacing = custom_pacing)
  result <- cobra_get(li, "/tournaments/3/rounds/pairings_data")

  expect_true(result$ok)
  expect_true(isTRUE(result$body$ok))
  expect_identical(captured_pacing, custom_pacing)
})

test_that("a 5xx exhausting retries is a hard stop", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 503))
  li <- list(base_url = "https://example.test", pacing = list(min_delay_s = 0.01, max_delay_s = 0.01))
  expect_error(cobra_get(li, "/tournaments/3/rounds/pairings_data", max_retries = 2L), class = "netrunneR_cobra_http_error")
})

test_that("discover_cobra_recent_index() extracts unique tournament ids from a type page's HTML", {
  httr2::local_mocked_responses(function(req) {
    if (grepl("/tournaments/type/1$", req$url)) {
      return(httr2::response(status_code = 200, body = charToRaw(
        '<a href="/tournaments/101">A</a><a href="/tournaments/205">B</a><a href="/tournaments/101">A again</a>'
      )))
    }
    httr2::response(status_code = 404)
  })
  li <- cobra_fixture_lineage(withr::local_tempdir())
  recent_index <- discover_cobra_recent_index(li)

  expect_setequal(recent_index$tournament_id, c(101L, 205L))
  expect_true(all(recent_index$tournament_type_id == 1L))
})

test_that("scrape_cobra_bundle() reports exists = FALSE when pairings_data 404s", {
  httr2::local_mocked_responses(function(req) httr2::response(status_code = 404))
  li <- cobra_fixture_lineage(withr::local_tempdir())
  bundle <- scrape_cobra_bundle(li, 999L)
  expect_false(bundle$exists)
})

test_that("fetch_cobra() discovers, refreshes, and backfills into a checkpointed pool", {
  httr2::local_mocked_responses(cobra_single_tournament_mock())

  store_root <- withr::local_tempdir()
  li <- cobra_fixture_lineage(store_root)
  attempt_dir <- withr::local_tempdir()

  staged <- fetch_cobra(li, attempt_dir)

  expect_true("3" %in% names(staged$bundles))
  expect_identical(staged$bundles[["3"]]$pairings_data$tournament$name, "Fixture Cup")
  expect_true(fs::file_exists(file.path(store_root, "objects", "3.json")))
  expect_true(fs::file_exists(file.path(store_root, "cobra-backfill-checkpoint.rds")))
  expect_true(fs::file_exists(file.path(store_root, "cobra-crawl-state.json")))

  crawl_state <- jsonlite::fromJSON(file.path(store_root, "cobra-crawl-state.json"), simplifyVector = TRUE)
  # max_known_id advances to the tail probe's max_checked_id
  # (start_id + COBRA_TAIL_MISS_LIMIT - 1), not just the highest
  # discovered tournament id -- probe_cobra_new_tail() always checks a
  # full miss-limit-sized window before giving up for this run.
  expect_identical(as.integer(crawl_state$max_known_id), 3L + COBRA_TAIL_MISS_LIMIT)
  expect_true(isTRUE(crawl_state$backfill_complete))

  expect_true(all(vapply(staged$checks, function(x) identical(x$status, "pass"), logical(1))))
  expect_type(staged$content_identity, "character")
})

test_that("fetch_cobra() skips already-settled ids on a second run instead of refetching them", {
  request_count <- 0L
  httr2::local_mocked_responses(function(req) {
    request_count <<- request_count + 1L
    cobra_single_tournament_mock()(req)
  })

  store_root <- withr::local_tempdir()
  li <- cobra_fixture_lineage(store_root)

  first <- fetch_cobra(li, withr::local_tempdir())
  first_count <- request_count

  second <- fetch_cobra(li, withr::local_tempdir())

  # The second run still refreshes tournament 3 (recent/tail-found ids
  # are always refreshed) and still re-probes the tail and type pages,
  # but the historical backfill walk has nothing left to attempt --
  # crawl_state$backfill_next_id already exceeds max_known_id -- so it
  # issues no additional pairings_data requests for ids 1-2.
  expect_identical(second$content_identity, first$content_identity)
  expect_true(request_count > first_count)
})
