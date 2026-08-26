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

# roxygen2/pkgdown/devtools/covr/shinytest2 are dev-only Suggests (package
# maintainer tooling, never loaded by netrunneR itself or needed by its
# users) that renv.lock does not carry -- document.R installs roxygen2 ad
# hoc for that reason. Rather than installing all five into the check
# container just to satisfy R CMD check's default "every Suggests must be
# installed" rule, force the standard opt-out: a package with dev-only
# Suggests that aren't installed should still check cleanly.
rcmdcheck::rcmdcheck(args = c("--no-manual"), error_on = "warning",
                      env = c("_R_CHECK_FORCE_SUGGESTS_" = "false"))
