#' Shiny server entry point
#'
#' Receives `app_data` (built once per process by
#' `netrunneR::load_ice_breaker_app_data()`, see `inst/shiny-app/app.R`)
#' rather than resolving releases and recomputing matchups itself --
#' cardpool/implementation data is static for the life of the R process,
#' so recomputing per Shiny session (opening two SQLite connections and
#' rerunning the ice x breaker cross-join on every browser tab) would be
#' pure waste.
#'
#' @param app_data The value `netrunneR::load_ice_breaker_app_data()` returns.
app_server <- function(input, output, session, app_data) {
  if (!is.null(app_data$missing_lineages)) {
    output$main <- shiny::renderUI(startup_error_ui(app_data$missing_lineages))
    return(invisible(NULL))
  }

  cards <- app_data$cards
  matchup <- app_data$matchup

  # Selected-card state for the detail modal is owned here, once, for the
  # whole session. Both mod_card_browser_server() and
  # mod_matchup_explorer_server() receive the same setter and only ever
  # call it; neither instantiates its own copy of the detail module.
  selected_code <- shiny::reactiveVal(NULL)
  netrunneR::mod_card_detail_server("card_detail_modal", selected_code, cards)

  output$main <- shiny::renderUI({
    bslib::page_navbar(
      title = "ICE vs Breakers",
      bslib::nav_panel("Browse", netrunneR::mod_card_browser_ui("browser")),
      bslib::nav_panel("Matchup", netrunneR::mod_matchup_explorer_ui("matchup"))
    )
  })

  netrunneR::mod_card_browser_server("browser", cards, selected_code)
  netrunneR::mod_matchup_explorer_server("matchup", cards, matchup, selected_code)
}

#' Render the missing-release startup error screen
#' @keywords internal
startup_error_ui <- function(missing_lineages) {
  netrunneR::alert_box(sprintf(
    "No active release for: %s. Run a sync and promote before starting the app.",
    paste(missing_lineages, collapse = ", ")
  ), "danger")
}
