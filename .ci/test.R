source(".ci/restore.R")
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::test()
