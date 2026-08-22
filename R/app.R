#' Serve the packaged Shiny app
#'
#' Serves the packaged app from system.file('shiny-app', package =
#' 'netrunneR') and guards every ABR-sourced view with
#' stopifnot(has_attribution), so the required backlink to
#' alwaysberunning.net renders in the UI itself, not only in
#' documentation.
#'
#' @param ... Passed to shiny::shinyAppDir()'s options argument.
#'
#' run_app() and the sync container both open the current release
#' through this same package -- the reason netrunneR ships as an R
#' package rather than a set of standalone scripts.
#'
#' @export
run_app <- function(...) {
  app_dir <- system.file("shiny-app", package = "netrunneR")
  if (!nzchar(app_dir)) {
    rlang::abort("Could not find the shiny-app directory; is netrunneR installed?", class = "netrunneR_missing_app")
  }
  shiny::shinyAppDir(app_dir, options = list(...))
}

#' Guard an ABR-sourced view with a required attribution flag
#'
#' Every ABR-sourced view must be constructed with has_attribution = TRUE,
#' asserted with stopifnot() rather than left to a documentation
#' convention, so the required backlink to alwaysberunning.net cannot be
#' silently dropped by a future UI change.
#'
#' @param has_attribution Logical. TRUE if the view's UI renders the
#'   required alwaysberunning.net backlink.
#'
#' @export
require_abr_attribution <- function(has_attribution) {
  stopifnot(isTRUE(has_attribution))
  invisible(TRUE)
}
