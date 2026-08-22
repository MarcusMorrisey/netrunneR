# Bootstraps renv itself via remotes::install_github(), matching the
# Dockerfile (ref: DL-007): rocker/r-ver ships no renv and CRAN does not
# serve arbitrary historical versions by version string. Then restores the
# package's own declared Imports/Suggests from renv.lock.
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("remotes")
  remotes::install_github("rstudio/renv@v1.2.4")
}
renv::restore(project = ".", prompt = FALSE)
