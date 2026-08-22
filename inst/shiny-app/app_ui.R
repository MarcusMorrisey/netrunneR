#' Shiny UI entry point (stub)
#'
#' Full UI implementation is out of scope for this pass; this stub
#' establishes the entry point for the app's UI, rendering a placeholder
#' heading. Any view sourced from the abr lineage must render the
#' alwaysberunning.net backlink guarded by
#' netrunneR::require_abr_attribution() in R/app.R.
app_ui <- function() {
  shiny::fluidPage(
    shiny::h1("netrunneR")
  )
}
