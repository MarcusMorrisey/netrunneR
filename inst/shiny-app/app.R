# Entry point shiny::shinyAppDir() actually looks for by convention.
# app_ui.R/app_server.R alone (the pre-existing files in this directory)
# were never wired together -- shinyAppDir() requires app.R or a
# ui.R/server.R pair, and neither existed. This was silently broken: both
# entry points were documented as stubs, so nothing had exercised
# run_app() yet.
#
# Package functions below are called with explicit `netrunneR::` prefixes
# rather than attaching the package with library(netrunneR): this
# directory is sourced by shinyAppDir() outside the package namespace, so
# a bare name would not resolve -- matching the same netrunneR:: rationale
# already documented on mod_card_detail_ui() (R/mod_card_detail.R).
# library(netrunneR) would work too, but attaching the whole package to
# the search path from inside a packaged app's own entry point risks
# masking any exported name that collides with something else already
# attached in the host session -- explicit prefixes avoid that risk for
# the handful of calls this file and app_server.R actually make.
source("app_ui.R", local = TRUE)
source("app_server.R", local = TRUE)

# Loaded once per process, not per session: cardpool/implementation data
# is static for the life of the R process, so every browser session
# shares this one computation and one pair of SQLite reads rather than
# each session independently reopening both databases and rerunning the
# ice x breaker cross-join (see netrunneR::load_ice_breaker_app_data()).
app_data <- netrunneR::load_ice_breaker_app_data()

shinyApp(
  ui = app_ui(),
  server = function(input, output, session) app_server(input, output, session, app_data)
)
