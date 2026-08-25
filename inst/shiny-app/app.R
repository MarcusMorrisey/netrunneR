# Entry point shiny::shinyAppDir() actually looks for by convention.
# app_ui.R/app_server.R alone (the pre-existing files in this directory)
# were never wired together -- shinyAppDir() requires app.R or a
# ui.R/server.R pair, and neither existed. This was silently broken: both
# entry points were documented as stubs, so nothing had exercised
# run_app() yet.
#
# library(netrunneR), not individual `netrunneR::` prefixes: shinyAppDir()
# sources this directory's files in an environment outside the package
# namespace, so anything app_ui.R/app_server.R call from the package
# needs to be reachable as a bare name. Attaching the package to the
# search path here does that for every @export'd function, rather than
# prefixing each call site individually.
library(netrunneR)

source("app_ui.R", local = TRUE)
source("app_server.R", local = TRUE)

shinyApp(ui = app_ui(), server = app_server)
