# Confirms lock contention surfaces as a typed condition
# (netrunneR_lock_contention) rather than terminating the process.
#
# filelock locks are per-process: the same R process can typically
# re-acquire a lock it already holds (verified directly against this
# host's filelock build), so contention can't be simulated by calling
# acquire_lock() twice from this test's own process -- it must come from
# a genuinely separate OS process, held via callr::r_bg().
hold_lock_in_background <- function(store_root) {
  lock_path <- file.path(store_root, ".sync.lock")
  fs::dir_create(store_root)
  bg <- callr::r_bg(function(lock_path) {
    lock <- filelock::lock(lock_path, timeout = 0)
    Sys.sleep(30)
  }, args = list(lock_path = lock_path))

  deadline <- Sys.time() + 5
  repeat {
    probe <- filelock::lock(lock_path, timeout = 0)
    if (is.null(probe)) break
    filelock::unlock(probe)
    if (Sys.time() > deadline) {
      bg$kill()
      stop("background lock holder never acquired the lock")
    }
    Sys.sleep(0.05)
  }
  bg
}

test_that("run_sync() signals netrunneR_lock_contention rather than terminating the process", {
  store_root <- withr::local_tempdir()
  li <- new_lineage("cardpool", "git_mirror", store_root, schema_version = 1L,
                    build_module_path = "R/build-cardpool.R")

  bg <- hold_lock_in_background(store_root)
  withr::defer(bg$kill())

  expect_error(run_sync(li, mode = "scheduled"), class = "netrunneR_lock_contention")
})

test_that("acquire_lock() returns NULL rather than blocking when the lock is already held", {
  store_root <- withr::local_tempdir()

  bg <- hold_lock_in_background(store_root)
  withr::defer(bg$kill())

  expect_null(acquire_lock(store_root))
})
