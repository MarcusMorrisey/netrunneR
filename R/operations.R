#' Render a Shiny output's body, catching any error rather than letting it
#' surface as Shiny's default per-output red error banner (which is only
#' visible to someone looking at that browser tab at the moment it
#' happens).
#'
#' TODO: log the caught error into R/ledger.R's durable event log instead
#' of `message()` -- this was written without reading ledger.R's actual
#' append API, so wiring it in is left as a follow-up rather than guessed
#' at here.
#'
#' @param expr_fn A zero-argument function whose body may throw.
#' @param fallback_message Character shown in place of a thrown error.
#' @export
safe_render <- function(expr_fn, fallback_message = "Something went wrong displaying this.") {
  tryCatch(
    expr_fn(),
    error = function(e) {
      message(sprintf("[netrunneR ui_render_error] %s", conditionMessage(e)))
      shiny::tags$div(class = "alert alert-warning", fallback_message)
    }
  )
}
