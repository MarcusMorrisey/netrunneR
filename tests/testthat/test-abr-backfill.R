# Confirms the interruptible backfill in R/abr-backfill.R resumes from
# checkpoint rather than refetching already-resolved tournament ids.
test_that("run_abr_backfill() checkpoints resolved ids and skips them on a re-run", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      body = charToRaw(jsonlite::toJSON(list(entries = list()), auto_unbox = TRUE))
    )
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api",
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  result1 <- run_abr_backfill(li, c("t1", "t2"))
  expect_true(result1)
  expect_true(fs::file_exists(file.path(store_root, "objects", "t1.json")))

  checkpoint_before <- readRDS(file.path(store_root, "backfill-checkpoint.rds"))
  mtime_before <- fs::file_info(file.path(store_root, "objects", "t1.json"))$modification_time

  # A re-run over the same ids must not refetch anything already resolved.
  run_abr_backfill(li, c("t1", "t2"))
  mtime_after <- fs::file_info(file.path(store_root, "objects", "t1.json"))$modification_time
  expect_identical(mtime_before, mtime_after)
})

# Confirms read_backfill_object() (R/abr-backfill.R), fetch_abr()'s
# companion read path for the pool run_abr_backfill() writes, reads back
# exactly what was persisted.
test_that("read_backfill_object() reads back what run_abr_backfill() wrote to the pool", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      body = charToRaw(jsonlite::toJSON(list(note = "round-trip"), auto_unbox = TRUE))
    )
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api",
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  run_abr_backfill(li, "t1")

  expect_identical(read_backfill_object(li, "t1")$note, "round-trip")
})
