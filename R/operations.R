#' A styled Bootstrap alert div
#'
#' Every "nothing to show" / error state in the ice/breaker app used to
#' build its own `tags$div(class = "alert alert-...", ...)` ad hoc
#' (`mod_card_browser.R`, `mod_matchup_explorer.R`, `safe_render()`'s own
#' fallback, and `app_server.R`'s startup error) -- one shared helper
#' instead.
#' @param message Character shown in the alert.
#' @param type One of "info", "warning", "danger".
#' @export
alert_box <- function(message, type = c("info", "warning", "danger")) {
  type <- match.arg(type)
  shiny::tags$div(class = paste0("alert alert-", type), message)
}

#' Render a Shiny output's body, catching any error rather than letting it
#' surface as Shiny's default per-output red error banner (which is only
#' visible to someone looking at that browser tab at the moment it
#' happens).
#'
#' TODO: log the caught error into R/ledger.R's durable event log instead
#' of `message()` -- this was written without reading ledger.R's actual
#' append API, and `append_ledger()` is scoped to a specific lineage's
#' store_root, which a UI render error doesn't have, so wiring this in
#' properly needs a store_root-optional event sink, not just a direct
#' call. Left as a follow-up rather than guessed at here.
#'
#' @param expr_fn A zero-argument function whose body may throw.
#' @param fallback_message Character shown in place of a thrown error.
#' @export
safe_render <- function(expr_fn, fallback_message = "Something went wrong displaying this.") {
  tryCatch(
    expr_fn(),
    error = function(e) {
      message(sprintf("[netrunneR ui_render_error] %s", conditionMessage(e)))
      alert_box(fallback_message, "warning")
    }
  )
}

#' Build a Shiny.setInputValue() onclick attribute string
#'
#' So a clickable element (a card image, a table cell) sets a namespaced
#' input without a per-element observer -- used identically by
#' `mod_card_browser.R` and `mod_matchup_explorer.R`; factored out here
#' rather than each constructing the same JS string.
#' @param session The module's session (its `ns()` namespaces the input id).
#' @param input_id Character. Unnamespaced input id to set.
#' @param value Character. Value to set it to.
#' @return Character, for use as an `onclick` HTML attribute.
#' @keywords internal
click_sets_input <- function(session, input_id, value) {
  sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'})", session$ns(input_id), value)
}

#' Resolve one lineage's active release without loading any data, or NULL
#' @keywords internal
resolve_active_release <- function(lineage_name) {
  tryCatch(resolve_release(lineage(lineage_name)), error = function(e) NULL)
}

#' Resolve a lineage's active release and run one query against its
#' processed SQLite database
#'
#' Generalizes the connect/query/disconnect sequence
#' `cardpool_codes()` (`R/build-implementation.R`) and the ice/breaker
#' Shiny app both need -- factored out here rather than duplicated across
#' three call sites (cardpool_codes(), and two near-identical blocks that
#' used to live directly in app_server.R for cardpool and implementation).
#' @param lineage_name Character. One of `BUILTIN_LINEAGES`.
#' @param db_filename Character. SQLite filename under the release's
#'   `processed_dir` (e.g. `"cardpool.sqlite"`).
#' @param sql Character. Query to run.
#' @return A list with `release` (the `resolve_release()` result) and
#'   `data` (the query result), or `NULL` if there's no active release or
#'   the db file is missing.
#' @keywords internal
query_active_release <- function(lineage_name, db_filename, sql) {
  active <- resolve_active_release(lineage_name)
  if (is.null(active)) return(NULL)
  db_path <- file.path(active$processed_dir, db_filename)
  if (!fs::file_exists(db_path)) return(NULL)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  list(release = active, data = DBI::dbGetQuery(con, sql))
}

#' Load the data the ice/breaker Shiny app needs, once per process
#'
#' Hoisted out of `app_server()` (called once from `inst/shiny-app/app.R`,
#' not per session) so every browser session shares one computation and
#' one pair of SQLite reads rather than each session independently
#' reopening both databases and rerunning the ice x breaker cross-join --
#' cardpool/implementation data is static for the life of the R process.
#' @return A list with `cards` and `matchup`, or `missing_lineages`
#'   (character vector, non-NULL) if either required release is
#'   unavailable -- callers branch on `missing_lineages` before touching
#'   `cards`/`matchup`.
#' @export
load_ice_breaker_app_data <- function() {
  cardpool_result <- query_active_release("cardpool", "cardpool.sqlite", "SELECT * FROM card")
  implementation_result <- query_active_release("implementation", "implementation.sqlite", "SELECT * FROM ice_breaker_traits")

  if (is.null(cardpool_result) || is.null(implementation_result)) {
    return(list(missing_lineages = c(
      if (is.null(cardpool_result)) "cardpool",
      if (is.null(implementation_result)) "implementation"
    )))
  }

  matchup_overrides <- readr::read_csv(
    system.file("extdata", "matchup_overrides.csv", package = "netrunneR"),
    show_col_types = FALSE
  )

  matchup_result <- compute_ice_breaker_matchups(
    implementation_result$data, cardpool_result$data, matchup_overrides,
    cardpool_release_id = basename(cardpool_result$release$release_dir),
    implementation_release_id = basename(implementation_result$release$release_dir)
  )

  list(cards = cardpool_result$data, matchup = matchup_result$matchups, missing_lineages = NULL)
}
