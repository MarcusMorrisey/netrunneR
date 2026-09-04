# R/deck-compare.R - pure deck resolution and pairing.
#
# No HTTP, no I/O, no reactivity: everything here is a function of (deck, all_codes,
# matchup) and is testable with fixtures and no mock. Codes resolve on `code` and only on
# `code`; a title join yields zero matches silently rather than erroring.
#
# Unresolved codes report one state, "not in cardpool release <id>". That is a deliberate,
# user-confirmed reversal of the original spec's two-cause split, because the schema holds
# no reprint-alias column to compute it from. See DL-032 and the follow-up in R/README.md.

#' Resolve a fetched deck against the active cardpool release
#'
#' @param deck A list as returned by `fetch_deck()`: `id`, `name` and `cards`, a named
#'   integer vector of card code to quantity, the identity's own code included.
#' @param all_codes A tibble of `code`, `title`, `type_code` for every card in the
#'   active cardpool release, as returned by `load_ice_breaker_app_data()`.
#' @return A list of five code sets: `identity`, `ice`, `breakers`, `other_known`
#'   (present in `all_codes` but none of the above), and `unresolved` (absent from
#'   `all_codes` entirely).
#' @details The join is on `code` and only ever on `code`. A title join against this
#'   cardpool produces zero matches rather than an error, so it fails silently and looks
#'   like an empty deck. Refuses outright only on a zero-card deck or a deck whose
#'   identity resolves to nothing; an unresolved code narrows the comparison and never
#'   aborts it.
#'
#'   The identity is not read from the fetch: the live `/decklist/<id>` envelope carries
#'   no `identity` field (R/README.md's "Decklist mirroring was removed, not repaired"
#'   documented this exact gap first). Instead it is found here, among the codes already
#'   known to this release, via `type_code == "identity"` -- the same idiom
#'   `load_ice_breaker_app_data()` uses. (DL-045)
#'
#'   `unresolved` reports one honest state, "not in cardpool release <id>". It does not
#'   split version skew from a reprint alias. That split is a stated requirement of the
#'   original feature spec which this implementation deliberately reverses, with the
#'   user's confirmation, because cardpool.sql carries no previous-versions or
#'   reprint-alias column on card, printing, card_set or restriction_card - the split
#'   could only be inference presented as measurement. The schema gap is an open
#'   follow-up owned by the cardpool lineage; see R/README.md. (DL-032, DL-035)
#' @keywords internal
resolve_deck_codes <- function(deck, all_codes) {
  codes <- names(deck$cards)
  if (length(codes) == 0) {
    stop("resolve_deck_codes(): deck has zero cards", call. = FALSE)
  }

  known <- all_codes[all_codes$code %in% codes, , drop = FALSE]

  identity <- known$code[known$type_code == "identity"]
  if (length(identity) == 0) {
    stop("resolve_deck_codes(): no identity card found among this deck's known codes", call. = FALSE)
  }

  # ice_breaker_pool()'s type test (type_code == "ice" | type_code == "program"),
  # reused rather than reinvented -- its subtype half (has_card_subtype(keywords,
  # "Icebreaker")) cannot run here: all_codes carries no `keywords` column by
  # design (see all_codes's own doc comment in R/operations.R, DL-035), so every
  # `program` code known to the release is treated as a breaker candidate. That is
  # a safe superset, not a misclassification the caller ever sees: deck_matchups()
  # re-filters against compute_ice_breaker_matchups()'s own ice_code/breaker_code
  # columns, which already encode the real subtype-compatible pairing, so a
  # non-breaker program contributes no rows there.
  is_pool_type <- known$type_code == "ice" | known$type_code == "program"

  ice <- known$code[known$type_code == "ice"]
  breakers <- known$code[known$type_code == "program"]
  other_known <- known$code[!is_pool_type & known$type_code != "identity"]
  unresolved <- setdiff(codes, all_codes$code)

  list(identity = identity, ice = ice, breakers = breakers, other_known = other_known, unresolved = unresolved)
}

#' Pair a Corp deck's resolved ice against a Runner deck's resolved breakers
#'
#' @param matchup A matchup tibble as returned by `compute_ice_breaker_matchups()`.
#' @param corp_ice_codes Character vector of the Corp deck's resolved ice codes.
#' @param runner_breaker_codes Character vector of the Runner deck's resolved breaker
#'   codes.
#' @param corp_copy_counts Named integer vector of card code to quantity, as carried
#'   on the Corp deck's `cards`.
#' @return The rows of the precomputed matchup tibble whose `ice_code` is in the Corp
#'   deck's resolved ice and whose `breaker_code` is in the Runner deck's resolved
#'   breakers, with the column set of [compute_ice_breaker_matchups()] unchanged, plus a
#'   separate named integer vector of ice copy counts keyed by code.
#' @details Nothing is recomputed: `cost_to_break`, `credit_differential` and `source`
#'   arrive as compute_ice_breaker_matchups() left them, `not_computable` included.
#'   Copy counts are returned alongside rather than as a column because both values are
#'   per-title properties invariant to how many copies a deck runs; a copy column would
#'   be a field the single-card modal must carry and ignore. (DL-038, DL-032)
#' @keywords internal
deck_matchups <- function(matchup, corp_ice_codes, runner_breaker_codes, corp_copy_counts) {
  rows <- matchup[
    matchup$ice_code %in% corp_ice_codes & matchup$breaker_code %in% runner_breaker_codes,
    ,
    drop = FALSE
  ]

  copy_counts <- corp_copy_counts[names(corp_copy_counts) %in% corp_ice_codes]

  list(matchups = rows, copy_counts = copy_counts)
}
