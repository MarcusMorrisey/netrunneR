# All R CMD check / devtools::test / roxygen2 targets run inside a rocker
# container against the bind-mounted working tree. (ref: DL-007)
R_IMAGE := rocker/r-ver:4.3.2
DOCKER_RUN := docker run --rm -v $(CURDIR):/pkg -w /pkg $(R_IMAGE)
# Same system libraries the Dockerfile installs, since the R packages built
# from source here need their headers too (git/gert, sqlite/RSQLite,
# curl/httr2, plus libicu/libxml2/libx11 pulled in transitively by
# stringi/xml2/clipr when no matching binary is available).
APT_INSTALL := apt-get update -qq && apt-get install -y --no-install-recommends -qq curl git libgit2-dev libsqlite3-dev libcurl4-openssl-dev libssl-dev libicu-dev libxml2-dev libx11-dev libuv1-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libwebp-dev > /dev/null

.PHONY: check test document coverage docs-current

check:
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/check.R'

test:
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/test.R'

document:
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/document.R'

# Fails if man/ or NAMESPACE in git differ from what roxygen2 generates.
# Compares against HEAD rather than against roxygen's own output, since
# `make document` leaves the tree self-consistent but uncommitted -- the
# exact state that once shipped a NAMESPACE missing five exports.
docs-current:
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/docs-current.R'

coverage:
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/coverage.R'
