# R/fetch-deck.R - NetrunnerDB decklist fetch.
#
# The network half of Deck Compare lives here and not in deck-compare.R for one concrete
# reason: tests/testthat/test-capture-boundary.R runs its no-response-bodies-on-disk AST
# scan over the glob R/*fetch*.R. A file named otherwise would sit outside that gate with
# nothing failing to say so. Keep every httr2 call for decks in this file. (DL-033)
#
# Nothing here persists anything. Decks are ephemeral inputs, not a sixth lineage; see
# R/README.md on why that distinction is what separates this from the removed
# decklist-mirroring feature.

#' Parse a bare deck id or NetrunnerDB deck URL into an id
#'
#' @param ref A bare numeric deck id or a netrunnerdb.com decklist/deck URL.
#' @return The deck id as a character scalar, or `NULL` if `ref` is `NULL` or empty.
#' @keywords internal
parse_deck_ref <- function(ref) {
  if (is.null(ref) || !nzchar(ref)) return(NULL)

  frame <- strsplit(ref, "decklist/|deck/view/|deck/")[[1]]
  id_part <- frame[length(frame)]
  strsplit(id_part, "/")[[1]][1]
}

#' Fetch a NetrunnerDB decklist
#'
#' @param ref A bare numeric deck id or a netrunnerdb.com deck/decklist URL.
#' @param lineage The `nrdb` lineage entry, supplying base_url and pacing only.
#' @return A list of `id`, `name` and `cards`, where `cards` is a named integer vector of
#'   card code to quantity, the identity's own code included at quantity 1. `NULL` ref
#'   yields `NULL` rather than an error.
#' @details Routes through [nrdb_get()] so the NRDB_CONTACT user agent, `pacing_rate()`
#'   throttling and the five-try retry come from the registry rather than a second httr2
#'   pipeline. The response body is memory-only via `capture_response_body()`; nothing is
#'   written to disk, and this file's name keeps it inside test-capture-boundary.R's
#'   `R/*fetch*.R` AST scan, which is why the network half is not in deck-compare.R.
#'   (DL-033, DL-039)
#'
#'   Does not read an `identity` field: the live `/decklist/<id>` envelope carries no such
#'   field, and never has -- this is the exact gap R/README.md's "Decklist mirroring was
#'   removed, not repaired" section already recorded ("an `identity_code` with no
#'   upstream source at all") before Deck Compare re-introduced the same assumption. The
#'   identity card is still identifiable: it is present in `cards` like any other card, at
#'   quantity 1, and [resolve_deck_codes()] finds it there via the cardpool's own
#'   `type_code == "identity"`, the same idiom `load_ice_breaker_app_data()` already uses.
#'   (DL-045)
#' @keywords internal
fetch_deck <- function(ref, lineage) {
  id <- parse_deck_ref(ref)
  if (is.null(id)) return(NULL)

  envelope <- nrdb_get(lineage, paste0("/decklist/", id))
  check_deck_envelope(envelope, id)

  deck <- as.list(envelope$data[1, ])
  cards <- unlist(deck$cards)
  storage.mode(cards) <- "integer"

  list(
    id = deck$id,
    name = deck$name,
    cards = cards
  )
}

#' Validate a decklist response envelope before any field is read
#'
#' @details Validates the decklist envelope as `success` identical to `TRUE`, `total`
#'   identical to `1`, and `data` present and non-empty, before any deck field is read.
#'   This is deliberately NOT [compare_shape()], which asserts only that a bare `data`
#'   list is present because that is the real shape of /reviews and /rulings. Applied to
#'   the decklist endpoint, compare_shape() would accept an error envelope carrying
#'   `success: false` alongside a `data` key. (DL-034)
#' @keywords internal
check_deck_envelope <- function(envelope, id) {
  if (!isTRUE(envelope$success)) {
    rlang::abort(sprintf("Deck %s: NRDB envelope did not report success", id))
  }
  if (!identical(envelope$total, 1L) && !identical(envelope$total, 1)) {
    rlang::abort(sprintf("Deck %s: NRDB envelope total was not 1", id))
  }
  if (is.null(envelope$data) || length(envelope$data) == 0) {
    rlang::abort(sprintf("Deck %s: NRDB envelope carried no data", id))
  }
  invisible(TRUE)
}
