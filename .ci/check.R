source(".ci/restore.R")
if (!requireNamespace("rcmdcheck", quietly = TRUE)) install.packages("rcmdcheck")
rcmdcheck::rcmdcheck(args = c("--no-manual"), error_on = "warning")
