# All R CMD check / devtools::test / roxygen2 targets run inside a rocker
# container against the bind-mounted working tree. (ref: DL-007)
R_IMAGE := rocker/r-ver:4.3.2
DOCKER_RUN := docker run --rm -v $(CURDIR):/pkg -w /pkg $(R_IMAGE)
# Restores the package's own declared Imports/Suggests from renv.lock before
# any target runs, since a fresh rocker container has none of them installed.
# renv itself is bootstrapped via remotes::install_github(), matching the
# Dockerfile (ref: DL-007): rocker/r-ver ships no renv and CRAN does not
# serve arbitrary historical versions by version string.
RESTORE_DEPS := if (!requireNamespace("renv", quietly = TRUE)) { install.packages("remotes"); remotes::install_github("rstudio/renv@v1.2.4") }; renv::restore(project = ".", prompt = FALSE)

.PHONY: check test document coverage

check:
	$(DOCKER_RUN) Rscript -e '$(RESTORE_DEPS); if (!requireNamespace("rcmdcheck", quietly = TRUE)) install.packages("rcmdcheck"); rcmdcheck::rcmdcheck(args = c("--no-manual"), error_on = "warning")'

test:
	$(DOCKER_RUN) Rscript -e '$(RESTORE_DEPS); if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools"); devtools::test()'

document:
	$(DOCKER_RUN) Rscript -e '$(RESTORE_DEPS); if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2"); roxygen2::roxygenise()'

coverage:
	$(DOCKER_RUN) Rscript -e '$(RESTORE_DEPS); if (!requireNamespace("covr", quietly = TRUE)) install.packages("covr"); covr::package_coverage()'
