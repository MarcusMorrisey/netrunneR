# Covers resolve_release()'s single-capture derivation of raw_dir/processed_dir.
test_that("resolve_release() derives raw_dir and processed_dir from one path_real() capture", {
  store_root <- withr::local_tempdir()
  target_dir <- file.path(store_root, "releases", "r1")
  fs::dir_create(target_dir)
  swap_active(store_root, target_dir)

  li <- new_lineage("cardpool", "git_mirror", store_root)
  resolved <- resolve_release(li)

  expect_identical(resolved$raw_dir, file.path(resolved$release_dir, "raw"))
  expect_identical(resolved$processed_dir, file.path(resolved$release_dir, "processed"))
})

test_that("resolve_release() aborts when no active release exists", {
  li <- new_lineage("cardpool", "git_mirror", withr::local_tempdir())
  expect_error(resolve_release(li), class = "netrunneR_no_active_release")
})
