# All R CMD check / devtools::test / roxygen2 targets run inside a rocker
# container against the bind-mounted working tree. (ref: DL-007)
R_IMAGE := rocker/r-ver:4.3.2

# Host directory holding the renv cache between runs. Without it every
# target re-downloads and re-installs all ~98 lockfile packages, because
# the cache lives at /root/.cache/R/renv INSIDE the container and dies
# with it -- only the working tree was ever mounted. Override with
# `make test RENV_CACHE=/some/path` if /home is short on space.
RENV_CACHE ?= $(HOME)/.cache/netrunneR-renv

HOST_UID := $(shell id -u)
HOST_GID := $(shell id -g)

DOCKER_RUN := docker run --rm \
  -v $(CURDIR):/pkg -w /pkg \
  -v $(RENV_CACHE):/root/.cache/R/renv \
  $(R_IMAGE)

# Same system libraries the Dockerfile installs, since the R packages built
# from source here need their headers too (git/gert, sqlite/RSQLite,
# curl/httr2, plus libicu/libxml2/libx11 pulled in transitively by
# stringi/xml2/clipr when no matching binary is available).
#
# libglpk-dev is here for igraph, which arrives as a dependency of
# treemap, which arrives as one of d3treeR -- the interactive treemap on
# the meta stats view. The p3m BINARY of igraph links libglpk.so.40, so
# this is needed even though nothing is compiled: without it restore
# fails at the load test with "unable to load shared object", several
# minutes into a run, naming a library nobody asked for.
#
# The last four are the spatial runtime libraries the Dockerfile already
# carries for sf, terra and tmap. THIS LIST HAD DRIFTED FROM THE
# DOCKERFILE: the map work added them there and not here, so a restore
# into a cold cache died on terra with "libproj.so.22: cannot open
# shared object file". It went unseen because a warm cache links terra
# straight from it and never runs renv's load test -- so the gap only
# appears once something else in the lockfile changes. Runtime packages
# rather than -dev, for the reason the Dockerfile gives at length: p3m
# serves these as binaries, so no header is ever needed.
APT_INSTALL := apt-get update -qq && apt-get install -y --no-install-recommends -qq curl git libgit2-dev libsqlite3-dev libcurl4-openssl-dev libssl-dev libicu-dev libxml2-dev libx11-dev libuv1-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libwebp-dev libglpk-dev libgdal30 libgeos-c1v5 libproj22 libudunits2-0 > /dev/null

# The container runs as root, unlike the production sync container, which
# runs 1000:1000 (ref: DL-002). It has to: APT_INSTALL above runs at
# container start, and apt-get needs root. The production image installs
# those libraries at BUILD time, so its runtime process never does.
#
# The cost is that everything the container writes into the bind-mounted
# tree -- man/*.Rd and NAMESPACE from roxygen, inst/pkg-src from
# .ci/check.R -- lands owned by root on the host. That has bitten twice:
# a `git pull` refused to overwrite root-owned man/ pages, and `rm` on
# them prompted for write-protected files.
#
# So each target hands the tree back afterwards. It runs unconditionally
# (`;` not `&&`) and preserves the R command's exit status, so a failing
# check still fails the target rather than being masked by a successful
# chown.
RECLAIM := ; status=$$?; chown -R $(HOST_UID):$(HOST_GID) /pkg; exit $$status

.PHONY: check test document coverage docs-current

# Docker would otherwise create a missing cache directory itself, owned
# by root, reintroducing exactly the permission problem RECLAIM fixes.
$(RENV_CACHE):
	mkdir -p $@

check: | $(RENV_CACHE)
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/check.R $(RECLAIM)'

test: | $(RENV_CACHE)
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/test.R $(RECLAIM)'

document: | $(RENV_CACHE)
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/document.R $(RECLAIM)'

# Fails if man/ or NAMESPACE in git differ from what roxygen2 generates.
# Compares against HEAD rather than against roxygen's own output, since
# `make document` leaves the tree self-consistent but uncommitted -- the
# exact state that once shipped a NAMESPACE missing five exports.
docs-current: | $(RENV_CACHE)
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/docs-current.R $(RECLAIM)'

coverage: | $(RENV_CACHE)
	$(DOCKER_RUN) bash -c '$(APT_INSTALL) && Rscript .ci/coverage.R $(RECLAIM)'
