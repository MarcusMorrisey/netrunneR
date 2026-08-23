source(".ci/restore.R")
if (!requireNamespace("rcmdcheck", quietly = TRUE)) install.packages("rcmdcheck")

# Mirrors the Dockerfile's pre-install step so R CMD check exercises the
# same build_revision() code path production actually runs: an installed
# package (via R CMD INSTALL, which R CMD check performs internally) has
# no R/*.R source files or top-level renv.lock, only pkg-src/ if something
# put it there first. Never committed -- see .gitignore.
if (!dir.exists("inst/pkg-src")) {
  dir.create("inst/pkg-src", recursive = TRUE)
  file.copy("R", "inst/pkg-src", recursive = TRUE)
  file.copy("renv.lock", "inst/pkg-src/renv.lock")
}

rcmdcheck::rcmdcheck(args = c("--no-manual"), error_on = "warning")
