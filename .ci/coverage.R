source(".ci/restore.R")
if (!requireNamespace("covr", quietly = TRUE)) install.packages("covr")

# See test.R's comment: covr::package_coverage() also runs tests against a
# source-loaded copy of the package, so it needs the same pkg-src/renv.lock
# staging build_view_manifest() (R/views-matchups.R) depends on.
if (!dir.exists("inst/pkg-src")) {
  dir.create("inst/pkg-src", recursive = TRUE)
  file.copy("R", "inst/pkg-src", recursive = TRUE)
  file.copy("renv.lock", "inst/pkg-src/renv.lock")
}

covr::package_coverage()
