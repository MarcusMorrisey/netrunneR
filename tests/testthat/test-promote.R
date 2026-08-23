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

test_that("swap_active() re-points active atomically with no absent window", {
  store_root <- withr::local_tempdir()
  old_dir <- file.path(store_root, "releases", "release-1")
  new_dir <- file.path(store_root, "releases", "release-2")
  fs::dir_create(old_dir)
  fs::dir_create(new_dir)
  swap_active(store_root, old_dir)
  active_link <- file.path(store_root, "active")

  delete_called_on_active <- FALSE
  rename_calls_to_active <- 0L

  testthat::local_mocked_bindings(
    file_delete = function(path, ...) {
      if (any(fs::path_norm(path) == fs::path_norm(active_link))) {
        delete_called_on_active <<- TRUE
      }
      NULL
    },
    file_move = function(path, new_path, ...) {
      if (identical(fs::path_norm(new_path), fs::path_norm(active_link))) {
        rename_calls_to_active <<- rename_calls_to_active + 1L
        expect_identical(basename(fs::path_real(active_link)), "release-1")
      }
      # Call the real rename directly rather than fs::file_move(): with
      # .package = "fs", the mock replaces file_move in fs's own
      # namespace too, so calling fs::file_move() here would re-enter
      # this same mock and recurse until the C stack overflows.
      file.rename(path, new_path)
    },
    .package = "fs"
  )

  swap_active(store_root, new_dir)

  expect_false(delete_called_on_active)
  expect_identical(rename_calls_to_active, 1L)
  expect_identical(basename(fs::path_real(active_link)), "release-2")
})

test_that("rollback() aborts on a release_id with no directory on disk", {
  store_root <- withr::local_tempdir()
  li <- new_lineage("cardpool", "git_mirror", store_root)
  expect_error(rollback(li, "no-such-release"), class = "netrunneR_no_such_release")
})
