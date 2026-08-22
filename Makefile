# All R CMD check / devtools::test / roxygen2 targets run inside a rocker
# container against the bind-mounted working tree. (ref: DL-007)
R_IMAGE := rocker/r-ver:4.3.2
DOCKER_RUN := docker run --rm -v $(CURDIR):/pkg -w /pkg $(R_IMAGE)

.PHONY: check test document coverage

check:
	$(DOCKER_RUN) Rscript -e 'if (!requireNamespace("rcmdcheck", quietly = TRUE)) install.packages("rcmdcheck"); rcmdcheck::rcmdcheck(args = c("--no-manual"), error_on = "warning")'

test:
	$(DOCKER_RUN) Rscript -e 'if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools"); devtools::test()'

document:
	$(DOCKER_RUN) Rscript -e 'if (!requireNamespace("roxygen2", quietly = TRUE)) install.packages("roxygen2"); roxygen2::roxygenise()'

coverage:
	$(DOCKER_RUN) Rscript -e 'if (!requireNamespace("covr", quietly = TRUE)) install.packages("covr"); covr::package_coverage()'
