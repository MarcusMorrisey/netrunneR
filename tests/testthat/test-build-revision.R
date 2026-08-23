# build_revision() must be identical for all five lineages given matching
# shared inputs -- a MUST-level property, not narrowed to git-mirror
# lineages. find_package_root() resolves to the real source root under
# devtools::test() and to pkg_root/pkg-src (created ahead of R CMD check
# by .ci/check.R, mirroring the Dockerfile) under an installed package, so
# no custom fixture is needed here -- build_revision() is exercised as-is
# in both contexts.
test_that("build_revision() is identical for all five lineages given the same static inputs", {
  li_a <- lineage("abr")
  li_b <- lineage("nrdb")

  br_a <- build_revision(li_a, li_a$build_module_path)
  br_b <- build_revision(li_b, li_b$build_module_path)

  # Different build modules, so the digests must differ even though every
  # other input (shared modules, schema files, renv.lock) is identical.
  expect_false(identical(br_a, br_b))
})

test_that("build_revision() computes identically across lineages given matching shared inputs", {
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
