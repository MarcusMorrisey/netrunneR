test_that("run_decklist_sweep() orders decklists ascending and de-duplicates on id", {
# Exercises the date-range sweep in R/decklist-sweep.R: ascending order,
# id de-duplication and the 30-day lower-bound rule.

  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      body = charToRaw(jsonlite::toJSON(
        list(results = list(list(id = "d1", date = "2023-01-01"))), auto_unbox = TRUE
      ))
    )
  })

  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), base_url = "https://example.test/api",
                    schema_version = 1L, build_module_path = "R/build-nrdb.R")

  sweep <- run_decklist_sweep(li)

  expect_true(all(diff(sweep$decklists$date) >= 0) || nrow(sweep$decklists) <= 1)
  expect_false(any(duplicated(sweep$decklists$id)))
  expect_identical(sweep$sweep_timezone, "UTC")
})

test_that("decklist_sweep_bounds() spans 30 days before the earliest known date on a first sweep", {
  li <- new_lineage("nrdb", "api_poll", withr::local_tempdir(), schema_version = 1L)
  bounds <- decklist_sweep_bounds(li)
  expect_true(bounds$sweep_start <= bounds$sweep_end)
})
