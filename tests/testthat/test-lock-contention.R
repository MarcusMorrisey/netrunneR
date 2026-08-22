# Confirms lock contention surfaces as a typed condition
# (netrunneR_lock_contention) rather than terminating the process.
test_that("run_sync() signals netrunneR_lock_contention rather than terminating the process", {
  store_root <- withr::local_tempdir()
  li <- new_lineage("cardpool", "git_mirror", store_root, schema_version = 1L,
                    build_module_path = "R/build-cardpool.R")

  held_lock <- acquire_lock(store_root)
  withr::defer(filelock::unlock(held_lock))

  expect_error(run_sync(li, mode = "scheduled"), class = "netrunneR_lock_contention")
})

test_that("acquire_lock() returns NULL rather than blocking when the lock is already held", {
  store_root <- withr::local_tempdir()
  held_lock <- acquire_lock(store_root)
  withr::defer(filelock::unlock(held_lock))
  expect_null(acquire_lock(store_root))
})
