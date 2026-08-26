# [integration] -- drives inst/shiny-app/app.R against a temporary
# promoted store built by local_store_fixture()
# (tests/testthat/helper-store-fixture.R).
#
# This suite was stubbed with an unconditional skip() until the store
# root became reachable: app.R calls netrunneR::load_ice_breaker_app_data(),
# which resolves through lineage(), which had "/data" hardcoded. The
# NETRUNNER_STORE_BASE seam (store_base(), R/lineage.R) is what allows a
# test to point the app at a temporary directory instead.
#
# shinytest2 launches the app in a background R process. That process
# inherits this one's environment, which is how NETRUNNER_STORE_BASE
# reaches it -- so the fixture must be created BEFORE the AppDriver.
#
# The app directory is taken from system.file() rather than a source
# path: these tests run against the installed package, where inst/ has
# been flattened into the package root.
#
# DELIBERATELY LEFT SKIPPING. shinytest2 is not in renv.lock, so
# .ci/restore.R never installs it and skip_if_not_installed() fires on
# every run. That is a decision, not an oversight: `make test` runs
# `docker run --rm` against stock rocker/r-ver with no persisted renv
# cache (Makefile), so the dependency would be re-fetched EVERY run, not
# once into an image. Measured cost, 2026-08-26:
#
#   shinytest2 + chromote + websocket + checkmate + globals   ~6.4 MiB
#   google-chrome-stable .deb            134 MiB dl / 433 MiB installed
#   Chrome for Testing, full             185 MiB dl
#   Chrome for Testing headless shell    114 MiB dl / 260 MiB unpacked
#
# The .deb additionally pulls libgtk-3-0/libnss3/libasound2/libcups2 and
# friends, none of which are in the Makefile's APT_INSTALL line.
#
# Judged not worth 120-190 MiB per run against a suite that completes in
# under 10 seconds. To turn these on: renv::snapshot() shinytest2 into
# the lock and install a Chrome in the test container. Whether
# chrome-headless-shell (the cheapest option) satisfies chromote's
# find_chrome() is UNVERIFIED -- check that before assuming the 114 MiB
# figure is the one that applies.

app_dir <- function() system.file("shiny-app", package = "netrunneR")

test_that("no active release renders the startup error and never the real tabs", {
  skip_if_not_installed("shinytest2")

  local_store_fixture(character(0))
  driver <- shinytest2::AppDriver$new(app_dir(), name = "no-release")
  withr::defer(driver$stop())

  html <- driver$get_html("body")
  # Exact text from startup_error_ui() (inst/shiny-app/app_server.R),
  # which names every missing lineage.
  expect_match(html, "No active release for: cardpool, implementation", fixed = TRUE)
  expect_false(grepl("ICE vs Breakers", html, fixed = TRUE))
})

test_that("one missing lineage is named on its own", {
  skip_if_not_installed("shinytest2")

  local_store_fixture("cardpool")
  driver <- shinytest2::AppDriver$new(app_dir(), name = "partial-release")
  withr::defer(driver$stop())

  html <- driver$get_html("body")
  expect_match(html, "No active release for: implementation", fixed = TRUE)
  expect_false(grepl("cardpool", html, fixed = TRUE))
})

test_that("a fully promoted store renders the real tabs and no error", {
  skip_if_not_installed("shinytest2")

  local_store_fixture()
  driver <- shinytest2::AppDriver$new(app_dir(), name = "promoted-release")
  withr::defer(driver$stop())

  html <- driver$get_html("body")
  # Title and nav_panel labels from app_server()'s page_navbar().
  expect_match(html, "ICE vs Breakers", fixed = TRUE)
  expect_match(html, "Browse", fixed = TRUE)
  expect_match(html, "Matchup", fixed = TRUE)
  expect_false(grepl("No active release for", html, fixed = TRUE))
})
