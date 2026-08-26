# [integration] -- requires shinytest2 (added to DESCRIPTION Suggests)
# and a temporary store root with promoted cardpool/implementation
# releases for inst/shiny-app/app.R's shinyApp() to resolve against.
# Sketched as the assertions this suite must make; wiring a temporary
# store root is left as a follow-up, since it depends on details of the
# container/store path setup (`/data/<name>`, per R/lineage.R) this
# change does not otherwise touch.

test_that("no active release renders the startup error and never the real tabs", {
  skip_if_not_installed("shinytest2")
  skip("TODO: promote no releases into a temporary store root, drive inst/shiny-app/app.R with shinytest2::AppDriver, assert the error text renders and browse/matchup tab markup does not")
})
