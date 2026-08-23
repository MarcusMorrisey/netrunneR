# Confirms run_sync()'s mode dispatch: mode only ever affects
# no_op_change()'s early-return (R/sync.R), never fetch_lineage()/
# build_lineage() themselves. mode = "scheduled" short-circuits to a
# no_change ledger record when staged content matches the active
# manifest; mode = "backfill" runs the identical fetch/build/validate/
# promote sequence but is never allowed to take that short-circuit, so
# it always proceeds through build_lineage() and promote() even against
# unchanged content.
#
# Uses a minimal stub lineage class with its own fetch_lineage()/
# build_lineage() S3 methods rather than a real lineage, so this test
# exercises run_sync()'s own dispatch logic in isolation from any one
# lineage's fetch/build detail.
test_stub_build_calls <- new.env()

fetch_lineage.netrunneR_test_stub <- function(lineage, attempt_dir, ...) {
  list(content_identity = "fixed-content", raw_dir = attempt_dir)
}

build_lineage.netrunneR_test_stub <- function(lineage, staged_raw, ...) {
  test_stub_build_calls$n <- test_stub_build_calls$n + 1L
  # Computed for real (not stubbed) so the value written to this run's
  # manifest matches what no_op_change() independently recomputes via
  # build_revision() on the next run -- exactly what every real build
  # method (e.g. build_abr()) does.
  list(build_revision = build_revision(lineage, lineage$build_module_path), release_id = NULL, checks = list())
}

# run_sync() calls fetch_lineage()/build_lineage() from inside the
# netrunneR namespace, whose UseMethod() dispatch does not fall back to
# this test file's environment -- registerS3method() adds the two stub
# methods above to netrunneR's own S3 methods table instead, exactly as
# NAMESPACE's S3method() directives do for the package's real methods.
registerS3method("fetch_lineage", "netrunneR_test_stub", fetch_lineage.netrunneR_test_stub, envir = asNamespace("netrunneR"))
registerS3method("build_lineage", "netrunneR_test_stub", build_lineage.netrunneR_test_stub, envir = asNamespace("netrunneR"))

make_stub_lineage <- function(store_root) {
  new_lineage("stub", "test_stub", store_root, schema_version = 1L,
              build_module_path = "R/build-cardpool.R")
}

test_that("mode = 'scheduled' short-circuits on unchanged content; mode = 'backfill' does not", {
  store_root <- withr::local_tempdir()
  li <- make_stub_lineage(store_root)
  test_stub_build_calls$n <- 0L

  # First run: no active release yet, so no_op_change() can't match and
  # this always proceeds through build_lineage()/promote().
  first <- run_sync(li, mode = "scheduled")
  expect_identical(first$event, "promoted")
  expect_identical(test_stub_build_calls$n, 1L)

  # Second scheduled run against identical content_identity and
  # build_revision: no_op_change() matches and short-circuits, so
  # build_lineage() must not run again.
  second <- run_sync(li, mode = "scheduled")
  expect_identical(second$event, "no_change")
  expect_identical(test_stub_build_calls$n, 1L)

  # A backfill run against that same unchanged content: no_op_change()
  # only ever fires for mode == "scheduled", so this must proceed through
  # build_lineage()/promote() despite nothing having changed upstream.
  third <- run_sync(li, mode = "backfill")
  expect_identical(third$event, "promoted")
  expect_identical(test_stub_build_calls$n, 2L)
})
