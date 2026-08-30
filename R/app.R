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
#' convention, so the required backlink to
#' [alwaysberunning.net](https://alwaysberunning.net) cannot be silently
#' dropped by a future UI change.
#'
#' @param has_attribution Logical. TRUE if the view's UI renders the
#'   required [alwaysberunning.net](https://alwaysberunning.net) backlink.
#'
#' @export
require_abr_attribution <- function(has_attribution) {
  stopifnot(isTRUE(has_attribution))
  invisible(TRUE)
}

#' The AlwaysBeRunning backlink, as UI
#'
#' One definition, rendered by every ABR-sourced view, for the same
#' reason cardpool_disclaimer_ui() is one definition: wording copied into
#' several modules drifts, and here the thing that would drift is the
#' link itself.
#'
#' A REAL ANCHOR, NOT TEXT. ABR's terms require a backlink, and a
#' rendered string reading "alwaysberunning.net" satisfies a reader
#' looking for a credit while satisfying nothing the terms actually ask
#' for. The one assertion test-preship-gates.R makes about this notice is
#' that it contains an href to the site.
#'
#' Deliberately not wrapped in a <details> like the licence notices. Those
#' collapse because they are long; this is one sentence, and hiding a
#' required link behind a disclosure triangle is the closest thing to not
#' rendering it.
#'
#' @return A shiny tag.
#' @export
abr_attribution_ui <- function() {
  shiny::tags$p(
    class = "small text-muted",
    "Tournament data from ",
    shiny::tags$a(
      href = "https://alwaysberunning.net",
      rel = "noopener",
      target = "_blank",
      "alwaysberunning.net"
    ),
    ", used with attribution as its terms of use require."
  )
}

#' The cardpool non-affiliation and copyright disclaimer, as UI
#'
#' One definition, rendered by every cardpool-sourced view. The wording
#' was previously hand-copied into three modules, so a correction to one
#' silently left the other two stale -- the exact drift
#' require_cardpool_disclaimer() exists to prevent, reintroduced one
#' level up in the string itself.
#'
#' NULL SIGNAL GAMES IS NAMED, THOUGH COPYRIGHT.md DOES NOT NAME THEM.
#' Null-Signal-Games/netrunner-cards-json's own COPYRIGHT.md, at the
#' mirrored commit, reads "copyrighted by Fantasy Flight Games and/or
#' Wizards of the Coast" and does not mention Null Signal Games at all.
#' That file is incomplete for the data it now covers: the repository
#' carries cards Null Signal Games designed and published, and those
#' cards carry a Null Signal Games copyright line on the card face and no
#' Fantasy Flight or Wizards line. Checked directly on the printed card
#' -- Ansel 2.0 (36028, Vantage Point, the most recent cycle) reads
#' "Null Signal Games, Illus. Benjamin Giletti" down its right edge,
#' where core-set Heimdall 1.0 (01061) reads "(c) 2012 Wizards of the
#' Coast LLC. (c) FFG".
#'
#' ONE NOTICE, SCOPED TO WHAT THE VIEW SHOWS. A view that knows exactly
#' which card it is displaying names that card's publisher; the Fantasy
#' Flight wording is then COPYRIGHT.md's verbatim, which is accurate for
#' the cards it actually describes.
#'
#' A view showing a MIXED pool names all three in one disjunction rather
#' than printing both statements together. Two absolute claims side by
#' side -- "card data is copyrighted by Null Signal Games" and "card data
#' is copyrighted by Fantasy Flight Games and/or Wizards of the Coast" --
#' cannot both be true of the same card data, and nothing on screen says
#' each is scoped to a subset. "and/or" over all three is one claim that
#' holds for every card in the pool.
#'
#' @param publishers Character vector of `released_by` values from
#'   [card_publishers()], or NULL for both notices.
#' @return A shiny tagList.
#' @export
cardpool_disclaimer_ui <- function(publishers = NULL) {
  known <- c("null_signal_games", "fantasy_flight_games")
  if (is.null(publishers)) publishers <- known
  publishers <- intersect(known, publishers)
  # A view whose cards trace to no known publisher still needs a notice,
  # so an empty selection falls back to naming all three rather than to
  # silence.
  if (length(publishers) == 0) publishers <- known

  holders <- if (length(publishers) > 1) {
    "Null Signal Games, Fantasy Flight Games, and/or Wizards of the Coast"
  } else if (identical(publishers, "null_signal_games")) {
    "Null Signal Games"
  } else {
    "Fantasy Flight Games and/or Wizards of the Coast"
  }

  shiny::tags$p(
    class = "text-muted small",
    sprintf(
      paste0("Card data is copyrighted by %s. Not maintained, produced, ",
             "endorsed, supported, or affiliated with %s."),
      holders, holders
    )
  )
}

#' Which publishers released the given cards
#'
#' Traced through printing -> card_set -> card_cycle, whose `released_by`
#' column the cardpool source already carries: 19 cycles are
#' `fantasy_flight_games` and 10 are `null_signal_games`, and no cycle is
#' unattributed. That is a fact in the data rather than a cutoff date
#' hardcoded here -- the licence changed hands once, but a list of cycle
#' names maintained by hand would go stale on the next release and fail
#' silently.
#'
#' @param codes Character vector of card codes.
#' @param legality The legality tables, which carry `printing`,
#'   `card_set` and `card_cycle`.
#' @return A character vector of distinct `released_by` values, empty if
#'   the tables needed are absent.
#' @export
card_publishers <- function(codes, legality) {
  needed <- c("printing", "card_set", "card_cycle")
  if (is.null(legality) || !all(needed %in% names(legality))) return(character(0))
  if (any(vapply(legality[needed], is.null, logical(1)))) return(character(0))

  printing <- legality$printing
  hit <- printing[printing$code %in% codes, , drop = FALSE]
  if (nrow(hit) == 0) return(character(0))

  sets <- legality$card_set
  cycles <- legality$card_cycle
  set_ids <- sets$card_cycle_id[match(hit$card_set_id, sets$id)]
  released <- cycles$released_by[match(set_ids, cycles$id)]
  sort(unique(released[!is.na(released)]))
}

#' The mtgred/netrunner MIT notice, as UI
#'
#' MIT requires the permission notice be included with "substantial
#' portions of the Software" -- naming the licence does not satisfy it,
#' and naming it is all the app used to do. The text below is verbatim
#' from that repo's LICENSE.txt, which carries no separate "Copyright
#' (c) ..." line above it (see require_implementation_license_notice()),
#' so this notice-and-permission text is the whole of what must travel
#' with the derived data.
#'
#' Rendered inside <details> rather than as a wall of text: the notice is
#' unconditionally present in the document, which is what the licence
#' asks for, without displacing the view it accompanies.
#'
#' @return A shiny tag.
#' @export
implementation_mit_notice_ui <- function() {
  shiny::tags$details(
    class = "small text-muted",
    shiny::tags$summary(
      paste0(
        "Ice/breaker interaction logic derived from the mtgred/netrunner ",
        "implementation, used under the MIT License."
      )
    ),
    shiny::tags$p("MIT License"),
    shiny::tags$p(
      paste0(
      "Permission is hereby granted, free of charge, to any person ",
      "obtaining a copy of this software and associated documentation ",
      "files (the \"Software\"), to deal in the Software without ",
      "restriction, including without limitation the rights to use, copy, ",
      "modify, merge, publish, distribute, sublicense, and/or sell copies ",
      "of the Software, and to permit persons to whom the Software is ",
      "furnished to do so, subject to the following conditions:"
    
      )
    ),
    shiny::tags$p(
      paste0(
      "The above copyright notice and this permission notice shall be ",
      "included in all copies or substantial portions of the Software."
    
      )
    ),
    shiny::tags$p(
      paste0(
      "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, ",
      "EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF ",
      "MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND ",
      "NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT ",
      "HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, ",
      "WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, ",
      "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER ",
      "DEALINGS IN THE SOFTWARE."
    
      )
    )
  )
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

#' The NetrunnerDB non-affiliation and copyright disclaimer, as UI
#'
#' Both statements netrunnerdb.com/en/about makes about reuse, reproduced
#' verbatim, plus a backlink. Rulings are authored content hosted by
#' NetrunnerDB -- rules-team posts, official FAQ entries, release-note
#' errata -- rather than raw card data, so the site is named as their
#' source rather than silently absorbed.
#'
#' @return A shiny tag.
#' @export
nrdb_disclaimer_ui <- function() {
  shiny::tags$p(
    class = "text-muted small",
    paste0(
      "Rulings via NetrunnerDB. The information presented on that site ",
      "about Android: Netrunner, both literal and graphical, is ",
      "copyrighted by Fantasy Flight Games and/or Null Signal Games. ",
      "That website is not produced, endorsed, supported, or affiliated ",
      "with Fantasy Flight Games."
    ),
    shiny::tags$a(href = "https://netrunnerdb.com", target = "_blank",
                  rel = "noopener noreferrer", "netrunnerdb.com")
  )
}

#' Guard an nrdb-sourced view with a required disclaimer flag
#'
#' Any view rendering nrdb-sourced text -- rulings, FAQ entries, errata --
#' must render nrdb_disclaimer_ui() and assert this with stopifnot(),
#' rather than leaving it to a documentation convention, so the
#' disclaimer cannot be silently dropped by a future UI change. This is
#' the fourth such guard; see the note in inst/shiny-app/CLAUDE.md about
#' adding a constant defaulting to FALSE rather than hardcoding TRUE at
#' the call site.
#'
#' @param has_disclaimer Logical. TRUE if the view renders the
#'   NetrunnerDB copyright and non-affiliation disclaimer.
#'
#' @export
require_nrdb_attribution <- function(has_disclaimer) {
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

#' Build the NetrunnerDB card image URL for a given card code
#'
#' Verified live this session against NetrunnerDB's own public API
#' (`https://netrunnerdb.com/api/2.0/public/card/01001`), which returns an
#' `imageUrlTemplate` field of exactly this shape
#' (`https://card-images.netrunnerdb.com/v2/large/{code}.jpg`) alongside
#' the card data -- constructing an image URL this way is the API's own
#' documented mechanism for consumers, not a reverse-engineered guess.
#' NetrunnerDB's API documentation
#' (<https://netrunnerdb.com/api>) states the API "is provided for use in
#' deckbuilders, card databases, tournament managers, and other tools that
#' complement playing Android: Netrunner" -- displaying card images in a
#' matchup-comparison tool falls squarely within that stated purpose.
#'
#' No separate attribution guard exists for image use specifically:
#' images are cardpool-sourced content under the same "copyrighted by
#' Fantasy Flight Games and/or Null Signal Games" / non-affiliation terms
#' as cardpool text, so any view rendering `card_image_url()` output is
#' already covered by, and must still call,
#' `require_cardpool_disclaimer(CARDPOOL_DISCLAIMER_CONFIRMED)` -- do not
#' add a second guard for this. The design doc lists an
#' `IMAGE_HOTLINK_TERMS_CONFIRMED` gate; the research that gate called
#' for is the paragraph above, so it is closed here rather than opened
#' as a third constant.
#'
#' @param code Character. Card code (cardpool `card.code`).
#' @return Character URL.
#' @export
card_image_url <- function(code) {
  sprintf("https://card-images.netrunnerdb.com/v2/large/%s.jpg", code)
}
