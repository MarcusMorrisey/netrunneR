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
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  result1 <- run_abr_backfill(li, c("t1", "t2"))
  expect_true(result1$all_settled)
  expect_identical(result1$permanent_ids, character(0))
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
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  run_abr_backfill(li, "t1")

  expect_identical(read_backfill_object(li, "t1")$note, "round-trip")
})

# Regression test for a real crash: the live ABR /tournaments/results
# endpoint returns id as a JSON number (confirmed live), so
# jsonlite::fromJSON() parses tournaments$id as an integer column, not
# character. Passing that straight through used to fail
# dplyr::bind_rows()/rows_upsert() the first time a checkpoint row was
# appended to the character(0)-seeded checkpoint tibble.
test_that("run_abr_backfill() accepts integer-typed tournament ids, matching the real API's JSON-number id field", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(
      status_code = 200,
      body = charToRaw(jsonlite::toJSON(list(entries = list()), auto_unbox = TRUE))
    )
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  result <- run_abr_backfill(li, c(101L, 102L))
  expect_true(result$all_settled)
  expect_true(fs::file_exists(file.path(store_root, "objects", "101.json")))

  checkpoint <- readRDS(file.path(store_root, "backfill-checkpoint.rds"))
  expect_type(checkpoint$tournament_id, "character")
})

# Regression test for a real crash: tournament 5379's /entries endpoint
# 500s in isolation on an otherwise-healthy server (confirmed live,
# curled directly). Previously any 5xx from abr_get() propagated
# uncaught and hard-stopped run_abr_backfill() entirely, blocking the
# rest of a ~4400-id backfill on one broken tournament. An isolated
# failure (not part of a run of ABR_BACKFILL_MAX_CONSECUTIVE_5XX) is now
# tombstoned instead, and the loop continues past it.
test_that("run_abr_backfill() tombstones an isolated 5xx and continues past it, rather than hard-stopping", {
  httr2::local_mocked_responses(function(req) {
    if (grepl("id=bad", req$url, fixed = TRUE)) {
      return(httr2::response(status_code = 500, body = charToRaw("boom")))
    }
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(entries = list()), auto_unbox = TRUE)))
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  result <- run_abr_backfill(li, c("good1", "bad", "good2"))

  expect_false(result$all_settled)
  expect_identical(result$permanent_ids, character(0))
  expect_true(fs::file_exists(file.path(store_root, "objects", "good1.json")))
  expect_true(fs::file_exists(file.path(store_root, "objects", "good2.json")))
  expect_false(fs::file_exists(file.path(store_root, "objects", "bad.json")))

  checkpoint <- readRDS(file.path(store_root, "backfill-checkpoint.rds"))
  bad_row <- checkpoint[checkpoint$tournament_id == "bad", ]
  expect_false(bad_row$resolved)
  expect_false(bad_row$permanent_unavailable)
  expect_identical(bad_row$last_failed_at, format(Sys.Date(), "%Y-%m-%d"))
})

# A tombstoned id is retried at most once per calendar day -- re-running
# the same day must not re-request it.
test_that("run_abr_backfill() does not re-attempt a tournament already tombstoned today", {
  request_count <- 0L
  httr2::local_mocked_responses(function(req) {
    request_count <<- request_count + 1L
    httr2::response(status_code = 500, body = charToRaw("boom"))
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  run_abr_backfill(li, "bad")
  expect_identical(request_count, 1L)

  run_abr_backfill(li, "bad")
  expect_identical(request_count, 1L)
})

# A run of ABR_BACKFILL_MAX_CONSECUTIVE_5XX failures in a row looks like
# a real outage, not isolated per-tournament breakage, and must still
# hard-stop rather than tombstone its way through the whole crawl.
test_that("run_abr_backfill() re-raises once consecutive 5xx failures look like an outage", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 500, body = charToRaw("boom"))
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  expect_error(
    run_abr_backfill(li, c("t1", "t2", "t3", "t4", "t5")),
    class = "netrunneR_abr_backfill_outage"
  )
})

# Regression test for a real false-positive: 5 already-known-bad ids
# (checkpointed from a prior run, e.g. tournament ids 5379, 4134, 2182,
# 1769, 1655 -- confirmed live, scattered non-adjacent ids each 5xx-ing
# in isolation on an otherwise-healthy server) retried together as a
# small batch must NOT trip the outage guard just because 3+ of them
# fail back-to-back within that retry batch. Each should instead be
# tombstoned/retried individually, same as a lone isolated failure.
test_that("run_abr_backfill() does not treat a retry batch of already-known-bad ids as an outage", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 500, body = charToRaw("boom"))
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  bad_ids <- c("5379", "4134", "2182", "1769", "1655")
  checkpoint_path <- file.path(store_root, "backfill-checkpoint.rds")
  fs::dir_create(store_root)
  saveRDS(
    tibble::tibble(
      tournament_id = bad_ids, resolved = FALSE,
      first_failed_at = as.character(Sys.Date() - 1),
      last_failed_at = as.character(Sys.Date() - 1),
      permanent_unavailable = FALSE
    ),
    checkpoint_path
  )

  result <- run_abr_backfill(li, bad_ids)

  expect_false(result$all_settled)
  expect_identical(result$permanent_ids, character(0))

  checkpoint <- readRDS(checkpoint_path)
  expect_true(all(!checkpoint$resolved))
  expect_true(all(checkpoint$last_failed_at == format(Sys.Date(), "%Y-%m-%d")))
})

# A batch mixing already-known-bad retries with fresh ids: the known-bad
# retries failing must not count toward the outage streak, but a run of
# ABR_BACKFILL_MAX_CONSECUTIVE_5XX consecutive failures among the fresh
# (first-time) ids must still trip the guard -- real outage protection
# for newly-attempted ids is unaffected by this fix.
test_that("run_abr_backfill() still detects an outage among fresh ids even when known-bad retries are interleaved", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 500, body = charToRaw("boom"))
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  checkpoint_path <- file.path(store_root, "backfill-checkpoint.rds")
  fs::dir_create(store_root)
  saveRDS(
    tibble::tibble(
      tournament_id = "known_bad", resolved = FALSE,
      first_failed_at = as.character(Sys.Date() - 1),
      last_failed_at = as.character(Sys.Date() - 1),
      permanent_unavailable = FALSE
    ),
    checkpoint_path
  )

  expect_error(
    run_abr_backfill(li, c("known_bad", "fresh1", "fresh2", "fresh3")),
    class = "netrunneR_abr_backfill_outage"
  )
})

# After ABR_BACKFILL_TOMBSTONE_DAYS of daily failures, a tournament is
# marked permanent_unavailable instead of blocking the release forever.
test_that("run_abr_backfill() marks a tournament permanent_unavailable once its tombstone ages past the retry window", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 500, body = charToRaw("boom"))
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  checkpoint_path <- file.path(store_root, "backfill-checkpoint.rds")
  fs::dir_create(store_root)
  saveRDS(
    tibble::tibble(
      tournament_id = "bad", resolved = FALSE,
      first_failed_at = as.character(Sys.Date() - ABR_BACKFILL_TOMBSTONE_DAYS),
      last_failed_at = as.character(Sys.Date() - 1),
      permanent_unavailable = FALSE
    ),
    checkpoint_path
  )

  result <- run_abr_backfill(li, "bad")

  expect_true(result$all_settled)
  expect_identical(result$permanent_ids, "bad")
})

# A checkpoint written before tombstoning existed only has
# tournament_id/resolved -- real progress (312 tournaments on
# mediaServer) predates this change and must not be discarded.
test_that("run_abr_backfill() migrates a pre-tombstoning checkpoint missing the new columns", {
  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(list(entries = list()), auto_unbox = TRUE)))
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api", pacing = list(min_delay_s = 2, max_delay_s = 2),
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  fs::dir_create(store_root)
  saveRDS(
    tibble::tibble(tournament_id = "old1", resolved = TRUE),
    file.path(store_root, "backfill-checkpoint.rds")
  )

  result <- run_abr_backfill(li, c("old1", "new1"))

  expect_true(result$all_settled)
  expect_true(fs::file_exists(file.path(store_root, "objects", "new1.json")))
  expect_false(fs::file_exists(file.path(store_root, "objects", "old1.json")))
})
