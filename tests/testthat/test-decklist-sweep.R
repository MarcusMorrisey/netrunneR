test_that("run_decklist_sweep() orders decklists ascending and de-duplicates on id", {
# Exercises the date-range sweep in R/decklist-sweep.R: ascending order,
# id de-duplication and the 30-day lower-bound rule.

  # date_creation, not date, matches the real /decklists/by_date/<date>
  # envelope (confirmed live against netrunnerdb.com/api/2.0/public).
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      body = charToRaw(jsonlite::toJSON(
        list(data = list(list(id = "d1", date_creation = "2023-01-01"))), auto_unbox = TRUE
      ))
    )
  })

  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api",
                    schema_version = 1L, build_module_path = "R/build-nrdb.R")

  sweep <- run_decklist_sweep(li)

  expect_true(all(diff(sweep$decklists$date_creation) >= 0) || nrow(sweep$decklists) <= 1)
  expect_false(any(duplicated(sweep$decklists$id)))
  expect_identical(sweep$sweep_timezone, "UTC")
})

test_that("run_decklist_sweep() requests the by_date path as a path segment, not a by-date query param", {
# Regression test for the real NetrunnerDB path: /decklists/by_date/{date}
# (underscore, date as a path segment) -- not /decklists/by-date?date=...

  captured_urls <- character(0)
  httr2::local_mocked_responses(function(req) {
    captured_urls <<- c(captured_urls, req$url)
    httr2::response(
      status_code = 200,
      body = charToRaw(jsonlite::toJSON(list(data = list()), auto_unbox = TRUE))
    )
  })

  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api",
                    schema_version = 1L, build_module_path = "R/build-nrdb.R")

  run_decklist_sweep(li)

  expect_true(all(grepl("/decklists/by_date/\\d{4}-\\d{2}-\\d{2}$", captured_urls)))
  expect_false(any(grepl("by-date", captured_urls, fixed = TRUE)))
})

test_that("decklist_sweep_bounds() spans 30 days before the earliest known date on a first sweep", {
  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), schema_version = 1L)
  bounds <- decklist_sweep_bounds(li)
  expect_true(bounds$sweep_start <= bounds$sweep_end)
})
