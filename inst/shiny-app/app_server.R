#' Shiny server entry point
#'
#' Loads the active cardpool and implementation releases directly (there
#' is no separate "matchup" release to load -- compute_ice_breaker_matchups()
#' is a plain function computed live from these two, exactly like
#' compute_identity_ratings() elsewhere in this package; `matchup` is not
#' one of the five BUILTIN_LINEAGES and has no promote/rollback machinery
#' of its own). Recomputes matchups fresh per session -- see the Follow-ups
#' note at the bottom of this file for the cross-session caching this
#' skips for now.
app_server <- function(input, output, session) {
  cardpool_active <- tryCatch(resolve_release(lineage("cardpool")), error = function(e) NULL)
  implementation_active <- tryCatch(resolve_release(lineage("implementation")), error = function(e) NULL)

  if (is.null(cardpool_active) || is.null(implementation_active)) {
    missing <- c(
      if (is.null(cardpool_active)) "cardpool",
      if (is.null(implementation_active)) "implementation"
    )
    output$main <- shiny::renderUI(startup_error_ui(missing))
    return(invisible(NULL))
  }

  card_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(cardpool_active$processed_dir, "cardpool.sqlite"))
  on.exit(DBI::dbDisconnect(card_con), add = TRUE)
  cards <- DBI::dbGetQuery(card_con, "SELECT * FROM card")

  impl_con <- DBI::dbConnect(RSQLite::SQLite(), file.path(implementation_active$processed_dir, "implementation.sqlite"))
  on.exit(DBI::dbDisconnect(impl_con), add = TRUE)
  ice_breaker_traits <- DBI::dbGetQuery(impl_con, "SELECT * FROM ice_breaker_traits")

  matchup_overrides <- readr::read_csv(
    system.file("extdata", "matchup_overrides.csv", package = "netrunneR"),
    show_col_types = FALSE
  )

  matchup_result <- compute_ice_breaker_matchups(
    ice_breaker_traits, cards, matchup_overrides,
    cardpool_release_id = basename(cardpool_active$release_dir),
    implementation_release_id = basename(implementation_active$release_dir)
  )
  matchup <- matchup_result$matchups

  # Selected-card state for the detail modal is owned here, once, for the
  # whole session. Both mod_card_browser_server() and
  # mod_matchup_explorer_server() receive the same setter and only ever
  # call it; neither instantiates its own copy of the detail module.
  selected_code <- shiny::reactiveVal(NULL)
  mod_card_detail_server("card_detail_modal", selected_code, cards)

  output$main <- shiny::renderUI({
    bslib::page_navbar(
      title = "ICE vs Breakers",
      bslib::nav_panel("Browse", mod_card_browser_ui("browser")),
      bslib::nav_panel("Matchup", mod_matchup_explorer_ui("matchup"))
    )
  })

  mod_card_browser_server("browser", cards, selected_code)
  mod_matchup_explorer_server("matchup", cards, matchup, selected_code)
}

#' Render the missing-release startup error screen
#' @keywords internal
startup_error_ui <- function(missing_lineages) {
  shiny::tags$div(class = "alert alert-danger",
    shiny::tags$p(sprintf(
      "No active release for: %s. Run a sync and promote before starting the app.",
      paste(missing_lineages, collapse = ", ")
    ))
  )
}

# Follow-ups (not implemented in this change):
# - cost_to_break/credit_differential are NA for every pair until
#   extract_ice_breaker_traits() does real per-card parsing (see
#   R/views-matchups.R's note on compute_cost_to_break_formula()).
# - matchups are recomputed on every new session; if this becomes a
#   measured startup-latency problem once real trait data exists,
#   compute_ice_breaker_matchups()'s manifest$cache_identity is already
#   suitable as a memoization key for a process-lifetime cache -- not
#   built here since there's nothing expensive to cache yet.
