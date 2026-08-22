# build_revision() must be identical for all five lineages given matching
# shared inputs -- a MUST-level property, not narrowed to git-mirror lineages.
#' Build a pkg-src fixture from the real source tree
#'
#' build_revision() reads inputs from pkg_root/pkg-src, a mirror the
#' Dockerfile creates only during the container build (mkdir -p
#' inst/pkg-src; cp -r R inst/pkg-src/R; cp renv.lock inst/pkg-src/renv.lock)
#' before R CMD INSTALL. That mirror never exists under `make test` or
#' devtools::test(), so gating these tests on its presence made them skip
#' in every normal test run instead of exercising the MUST-level property.
#' Assembling the same mirror in a temp dir from the package's own source
#' tree, and pointing find_package_root() at it, exercises build_revision()
#' for real without depending on the Docker build having run.
#' @keywords internal
local_pkg_src_fixture <- function(env = parent.frame()) {
  pkg_root <- system.file(package = "netrunneR")
  src_root <- if (nzchar(pkg_root) && fs::dir_exists(file.path(pkg_root, "R"))) {
    pkg_root
  } else {
    testthat::test_path("..", "..")
  }

  fixture_root <- withr::local_tempdir(.local_envir = env)
  fs::dir_create(file.path(fixture_root, "pkg-src"))
  fs::dir_copy(file.path(src_root, "R"), file.path(fixture_root, "pkg-src", "R"))
  fs::file_copy(file.path(src_root, "renv.lock"), file.path(fixture_root, "pkg-src", "renv.lock"))

  testthat::local_mocked_bindings(
    find_package_root = function() fixture_root,
    .env = env
  )

  fixture_root
}

test_that("build_revision() is identical for all five lineages given the same static inputs", {
  local_pkg_src_fixture()

  li_a <- lineage("abr")
  li_b <- lineage("nrdb")

  br_a <- build_revision(li_a, li_a$build_module_path)
  br_b <- build_revision(li_b, li_b$build_module_path)

  # Different build modules, so the digests must differ even though every
  # other input (shared modules, schema files, renv.lock) is identical.
  expect_false(identical(br_a, br_b))
})

test_that("build_revision() computes identically across lineages given matching shared inputs", {
  local_pkg_src_fixture()

  li_x <- new_lineage("fixture-x", "api_poll", "/tmp/unused-x", schema_version = 1L)
  li_y <- new_lineage("fixture-y", "git_mirror", "/tmp/unused-y", schema_version = 1L)

  br_x <- build_revision(li_x, "R/build-cardpool.R")
  br_y <- build_revision(li_y, "R/build-cardpool.R")

  # Holds build_module_path and schema_version constant across two
  # lineage objects that differ in name and source_type: build_revision()
  # must not vary with either, since it is a single lineage-agnostic
  # implementation rather than narrowed to any one lineage's identity.
  expect_identical(br_x, br_y)
})

test_that("build_revision() aborts loudly on a missing expected input rather than hashing raw(0)", {
  li <- new_lineage("cardpool", "git_mirror", "/tmp/unused", schema_version = 1L)
  expect_error(
    build_revision(li, "R/does-not-exist.R"),
    class = "netrunneR_build_revision_missing_input"
  )
})
