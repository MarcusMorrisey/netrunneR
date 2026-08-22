source(".ci/restore.R")
if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2")
roxygen2::roxygenise()
