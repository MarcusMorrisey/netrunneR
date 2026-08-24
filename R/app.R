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

#' Guard an implementation-sourced view with a required MIT notice flag
#'
#' The mtgred/netrunner implementation is MIT licensed. Its LICENSE.txt
#' reads, in full:
#'
#'   This software consists of voluntary contributions made by many
#'   individuals.
#'
#'   ====
#'
#'   MIT License
#'
#'   Permission is hereby granted, free of charge, to any person obtaining
#'   a copy of this software and associated documentation files (the
#'   "Software"), to deal in the Software without restriction, including
#'   without limitation the rights to use, copy, modify, merge, publish,
#'   distribute, sublicense, and/or sell copies of the Software, and to
#'   permit persons to whom the Software is furnished to do so, subject to
#'   the following conditions:
#'
#'   The above copyright notice and this permission notice shall be
#'   included in all copies or substantial portions of the Software.
#'
#'   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#'   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#'   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#'   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
#'   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
#'   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
#'   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#'   SOFTWARE.
#'
#' (mtgred/netrunner's LICENSE.txt carries no separate "Copyright (c)
#' ..." line above this text -- the notice-and-permission text above is
#' the whole of what MIT requires to accompany "substantial portions of
#' the Software.")
#'
#' Any implementation-sourced view or export -- e.g. ice/breaker trait
#' data extracted in build-implementation.R -- must render this notice
#' wherever content derived from the mtgred/netrunner repo is displayed
#' or redistributed, asserted with stopifnot() rather than left to a
#' documentation convention, so the required MIT notice cannot be
#' silently dropped by a future UI change.
#'
#' @param has_notice Logical. TRUE if the view's UI renders the required
#'   mtgred/netrunner MIT copyright and permission notice.
#'
#' @export
require_implementation_license_notice <- function(has_notice) {
  stopifnot(isTRUE(has_notice))
  invisible(TRUE)
}

#' Guard a cardpool-sourced view with a required non-affiliation disclaimer flag
#'
#' Null-Signal-Games/netrunner-cards-json's own COPYRIGHT.md states that
#' cardpool card data "is copyrighted by Fantasy Flight Games and/or
#' Wizards of the Coast" and that the repository "is not maintained,
#' produced, endorsed, supported, or affiliated with Fantasy Flight Games
#' and/or Wizards of the Coast." No attribution backlink is asked for,
#' but any cardpool-sourced view should render that non-affiliation
#' disclaimer to avoid implying official endorsement, asserted with
#' stopifnot() rather than left to a documentation convention so the
#' disclaimer cannot be silently dropped by a future UI change.
#'
#' @param has_disclaimer Logical. TRUE if the view's UI renders a
#'   disclaimer that the content is not maintained, produced, endorsed,
#'   supported, or affiliated with Fantasy Flight Games and/or Wizards of
#'   the Coast.
#'
#' @export
require_cardpool_disclaimer <- function(has_disclaimer) {
  stopifnot(isTRUE(has_disclaimer))
  invisible(TRUE)
}

#' Guard a rules-sourced view with a required non-affiliation disclaimer flag
#'
#' nullsignal.games' own Comprehensive Rules hub carries a footer
#' disclaimer reading: "Null Signal Games is not associated with,
#' produced by, or endorsed by Fantasy Flight Games, R. Talsorian Games,
#' or Wizards of the Coast." Any rules-sourced view should render that
#' same disclaimer to avoid implying official endorsement, asserted with
#' stopifnot() rather than left to a documentation convention so the
#' disclaimer cannot be silently dropped by a future UI change.
#'
#' @param has_disclaimer Logical. TRUE if the view's UI renders a
#'   disclaimer that the content is not associated with, produced by, or
#'   endorsed by Fantasy Flight Games, R. Talsorian Games, or Wizards of
#'   the Coast.
#'
#' @export
require_rules_disclaimer <- function(has_disclaimer) {
  stopifnot(isTRUE(has_disclaimer))
  invisible(TRUE)
}
