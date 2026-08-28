source(".ci/restore.R")
if (!requireNamespace("rcmdcheck", quietly = TRUE)) install.packages("rcmdcheck")

# Mirrors the Dockerfile's pre-install step so R CMD check exercises the
# same build_revision() code path production actually runs: an installed
# package (via R CMD INSTALL, which R CMD check performs internally) has
# no R/*.R source files or top-level renv.lock, only pkg-src/ if something
# put it there first. Never committed -- see .gitignore.
# Rebuilt from scratch on every run, never topped up. This was guarded by
# `if (!dir.exists("inst/pkg-src"))`, so the mirror was copied once and
# then never again: a working tree that had ever run `make check` kept
# checking against whatever R/ looked like that first time. It was found
# eight files behind master. Two failure modes, neither of which announces
# itself -- R CMD check exercising stale sources, and build_revision()
# (R/views-matchups.R) resolving pkg-src to stamp `analysis_revision` on
# release manifests, recording provenance for code that is not what ran.
#
# unlink() rather than file.copy(overwrite = TRUE): overwriting refreshes
# files that still exist but leaves behind ones deleted from R/, which is
# the same staleness in a quieter form.
unlink("inst/pkg-src", recursive = TRUE)
dir.create("inst/pkg-src", recursive = TRUE)
stopifnot(all(file.copy("R", "inst/pkg-src", recursive = TRUE)))
stopifnot(all(file.copy("renv.lock", "inst/pkg-src/renv.lock")))

# roxygen2/pkgdown/devtools/covr/shinytest2 are dev-only Suggests (package
# maintainer tooling, never loaded by netrunneR itself or needed by its
# users) that renv.lock does not carry -- document.R installs roxygen2 ad
# hoc for that reason. Rather than installing all five into the check
# container just to satisfy R CMD check's default "every Suggests must be
# installed" rule, force the standard opt-out: a package with dev-only
# Suggests that aren't installed should still check cleanly.
rcmdcheck::rcmdcheck(args = c("--no-manual"), error_on = "warning",
                      env = c("_R_CHECK_FORCE_SUGGESTS_" = "false"))
