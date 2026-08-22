# Bootstraps renv itself via remotes::install_github(), matching the
# Dockerfile (ref: DL-007): rocker/r-ver ships no renv and CRAN does not
# serve arbitrary historical versions by version string. Then restores the
# package's own declared Imports/Suggests from renv.lock.
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("remotes")
  remotes::install_github("rstudio/renv@v1.2.4")
}
# The rocker/r-ver image's own Rprofile.site pins CRAN to a date-frozen p3m.dev
# snapshot (2024-02-28) whose /src/contrib index does not carry the current
# package versions renv.lock actually pins (confirmed via direct 404s), while
# p3m.dev's "latest" alias serves the same packages as binaries successfully.
# Overriding repos here, not by editing Rprofile.site, keeps this local to the
# restore step.
renv::restore(project = ".", prompt = FALSE, repos = c(CRAN = "https://p3m.dev/cran/__linux__/jammy/latest"))
