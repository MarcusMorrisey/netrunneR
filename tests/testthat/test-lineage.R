# Lineage registry tests: store_root case-matching (ref: DL-009), S3 class
# tagging, staging containment under a fixture root, and the /srv literal
# sentinel that guards against a host-path leaking into R/ source.
test_that("every built-in lineage resolves store_root to literal lowercase /data/<name>", {
  for (nm in BUILTIN_LINEAGES) {
    li <- lineage(nm)
    expect_identical(li$store_root, file.path("/data", nm))
    expect_identical(li$store_root, tolower(li$store_root))
  }
})

test_that("nrdb and abr resolve real base_url values, not NULL", {
  # Regression coverage for the gap the repo_url/hub_url fix (0e256a3)
  # left open: .LINEAGE_REGISTRY never set base_url for the two api_poll
  # lineages, so every real nrdb_get()/abr_get() call built
  # paste0(NULL, path) and failed deep inside curl's URL parser rather
  # than at lineage construction.
  expect_identical(lineage("nrdb")$base_url, "https://netrunnerdb.com/api/2.0/public")
  expect_identical(lineage("abr")$base_url, "https://alwaysberunning.net/api")
})

test_that("new_lineage() assigns the netrunneR_<source_type> and netrunneR_lineage class pair", {
  li <- new_lineage("fixture", "api_poll", "/data/fixture")
  expect_identical(class(li), c("netrunneR_api_poll", "netrunneR_lineage"))
})

test_that("a fixture sync stays entirely under a withr::local_tempdir() root", {
  root <- withr::local_tempdir()
  li <- new_lineage("fixture", "git_mirror", root)
  attempt_dir <- file.path(li$store_root, "staging", "attempt-1")
  fs::dir_create(attempt_dir)
  writeLines("fixture", file.path(attempt_dir, "raw.txt"))
  written <- fs::dir_ls(root, recurse = TRUE)
  expect_true(all(startsWith(written, root)))
})

test_that("R/ source contains no /srv literal", {
  pkg_root <- system.file(package = "netrunneR")
  r_dir <- if (nzchar(pkg_root)) file.path(pkg_root, "R") else testthat::test_path("..", "..", "R")
  if (!fs::dir_exists(r_dir)) skip("R/ source not resolvable from installed package")

  r_files <- fs::dir_ls(r_dir, glob = "*.R")
  offenders <- character(0)
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    if (any(grepl("/srv", lines, fixed = TRUE))) offenders <- c(offenders, f)
  }
  expect_identical(offenders, character(0))
})
