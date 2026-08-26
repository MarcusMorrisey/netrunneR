# Covers promote()'s move-then-swap sequence and rollback()'s guard
# against a missing release_id.
test_that("promote() moves the staged directory and active points at it", {
  store_root <- withr::local_tempdir()
  staging_dir <- file.path(store_root, "staging", "attempt-1")
  fs::dir_create(staging_dir)
  writeLines("x", file.path(staging_dir, "marker.txt"))

  release_dir <- promote(store_root, staging_dir, "release-1")

  expect_true(fs::dir_exists(release_dir))
  expect_false(fs::dir_exists(staging_dir))
  expect_identical(basename(fs::path_real(file.path(store_root, "active"))), "release-1")
})

test_that("swap_active() re-points active on every call, not just the first", {
  # The regression test this file did not have. The previous version of
  # this test mocked fs::file_move() with file.rename() -- to avoid the
  # mock recursing into itself -- which meant it substituted the CORRECT
  # primitive for the buggy call and asserted the mock's behaviour rather
  # than swap_active()'s. Nothing here mocks anything.
  #
  # In production the consequence was four days of daily syncs recording
  # "promoted" in the ledger while `active` never moved off the first
  # release, because fs::file_move() resolves a symlink-to-a-directory
  # destination and moves INTO it.
  store_root <- withr::local_tempdir()
  old_dir <- file.path(store_root, "releases", "release-1")
  new_dir <- file.path(store_root, "releases", "release-2")
  fs::dir_create(old_dir)
  fs::dir_create(new_dir)
  active_link <- file.path(store_root, "active")

  # First swap: `active` does not exist yet. This always worked.
  swap_active(store_root, old_dir)
  expect_identical(basename(fs::path_real(active_link)), "release-1")

  # Second swap: `active` exists and points at a directory. This is the
  # case that silently did nothing.
  swap_active(store_root, new_dir)
  expect_identical(basename(fs::path_real(active_link)), "release-2")

  # A third, to catch a fix that only handles one replacement.
  swap_active(store_root, old_dir)
  expect_identical(basename(fs::path_real(active_link)), "release-1")
})

test_that("swap_active() leaves no temp symlink behind anywhere in the store", {
  # The visible symptom in production: one orphaned .active.tmp.* symlink
  # inside the stale release directory per failed promote, five of them
  # by the time it was noticed.
  store_root <- withr::local_tempdir()
  old_dir <- file.path(store_root, "releases", "release-1")
  new_dir <- file.path(store_root, "releases", "release-2")
  fs::dir_create(old_dir)
  fs::dir_create(new_dir)

  swap_active(store_root, old_dir)
  swap_active(store_root, new_dir)

  strays <- fs::dir_ls(store_root, all = TRUE, recurse = TRUE, type = "symlink")
  strays <- strays[grepl("[.]active[.]tmp[.]", basename(strays))]
  expect_length(strays, 0)
})

test_that("swap_active() replaces active without ever deleting it first", {
  # The property the removed test was reaching for: no window in which
  # `active` is absent. Only fs::file_delete is mocked -- never the
  # rename under test -- so the swap itself still runs for real.
  store_root <- withr::local_tempdir()
  old_dir <- file.path(store_root, "releases", "release-1")
  new_dir <- file.path(store_root, "releases", "release-2")
  fs::dir_create(old_dir)
  fs::dir_create(new_dir)
  swap_active(store_root, old_dir)
  active_link <- file.path(store_root, "active")

  delete_called_on_active <- FALSE
  testthat::local_mocked_bindings(
    file_delete = function(path, ...) {
      if (any(fs::path_norm(path) == fs::path_norm(active_link))) {
        delete_called_on_active <<- TRUE
      }
      NULL
    },
    .package = "fs"
  )

  swap_active(store_root, new_dir)

  expect_false(delete_called_on_active)
  expect_identical(basename(fs::path_real(active_link)), "release-2")
})

test_that("promote() activates each successive release, not just the first", {
  # promote() -> swap_active(), so the same defect meant a second
  # promote() moved the release into place and left it inactive.
  store_root <- withr::local_tempdir()
  for (id in c("release-1", "release-2")) {
    staging_dir <- file.path(store_root, "staging", id)
    fs::dir_create(staging_dir)
    writeLines(id, file.path(staging_dir, "marker.txt"))
    promote(store_root, staging_dir, id)
    expect_identical(basename(fs::path_real(file.path(store_root, "active"))), id)
  }
})

test_that("rollback() aborts on a release_id with no directory on disk", {
  store_root <- withr::local_tempdir()
  li <- new_lineage("cardpool", "git_mirror", store_root)
  expect_error(rollback(li, "no-such-release"), class = "netrunneR_no_such_release")
})
