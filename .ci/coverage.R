source(".ci/restore.R")
if (!requireNamespace("covr", quietly = TRUE)) install.packages("covr")
covr::package_coverage()
