# Card-pool and ban-list legality, derived from the cardpool lineage's
# rotation / rotation_cycle / mwl / mwl_card tables (see
# inst/sql/schema/cardpool.sql and read_rotations()/read_mwl() in
# R/build-cardpool.R).
#
# Every function here is pure: it takes already-loaded data frames and a
# reference date, and returns data frames. Nothing resolves a release or
# touches a database, so the app can compute legality once at startup
# alongside everything else it loads.

#' The rotation in force on a given date
#'
#' Upstream ships every rotation ever applied; the active one is simply
#' the most recent whose `date_start` has passed. Returns NULL when the
#' table is empty or every rotation is still in the future, which callers
#' must treat as "no rotation applies" rather than "nothing is legal".
#'
#' @param rotation The cardpool `rotation` table.
#' @param as_of Date. Defaults to today.
#' @return A one-row data frame, or NULL.
#' @export
active_rotation <- function(rotation, as_of = Sys.Date()) {
  if (!nrow(rotation)) return(NULL)
  started <- rotation[as.Date(rotation$date_start) <= as_of, , drop = FALSE]
  if (!nrow(started)) return(NULL)
  started[which.max(as.Date(started$date_start)), , drop = FALSE]
}

#' The most recent ban list for a format on a given date
#'
#' @param mwl The cardpool `mwl` table.
#' @param format Character. One of the derived formats (`"standard"`,
#'   `"startup"`, `"napd"`, `"sunset"`); defaults to Standard.
#' @param as_of Date. Defaults to today.
#' @return A one-row data frame, or NULL when that format has no list yet
#'   in force.
#' @export
active_mwl <- function(mwl, format = "standard", as_of = Sys.Date()) {
  if (!nrow(mwl)) return(NULL)
  candidates <- mwl[mwl$format == format & as.Date(mwl$date_start) <= as_of, , drop = FALSE]
  if (!nrow(candidates)) return(NULL)
  candidates[which.max(as.Date(candidates$date_start)), , drop = FALSE]
}

#' Annotate cards with rotation and ban-list status
#'
#' Adds four columns rather than filtering, so a UI can show a banned
#' card greyed out (and say why) instead of having it silently vanish --
#' the same reasoning behind `matchup.source` carrying
#' `"not_computable"` rather than dropping the row.
#'
#' * `in_rotation`  -- FALSE if the card's pack belongs to a rotated-out
#'   cycle. TRUE for every card when no rotation is supplied.
#' * `is_banned`    -- TRUE when the active list sets `deck_limit` to 0.
#' * `is_restricted` -- TRUE when the active list flags the card restricted.
#' * `influence_penalty` -- `universal_faction_cost` where the list sets
#'   one, otherwise 0.
#'
#' @param cards The cardpool `card` table.
#' @param packs The cardpool `pack` table (supplies each card's cycle).
#' @param rotation_cycle The cardpool `rotation_cycle` table.
#' @param mwl_card The cardpool `mwl_card` table.
#' @param rotation A one-row rotation from [active_rotation()], or NULL.
#' @param mwl A one-row ban list from [active_mwl()], or NULL.
#' @return `cards` with the four columns above added.
#' @export
annotate_legality <- function(cards, packs, rotation_cycle, mwl_card,
                              rotation = NULL, mwl = NULL) {
  cycle_of_card <- packs$cycle_code[match(cards$pack_code, packs$code)]

  cards$in_rotation <- if (is.null(rotation)) {
    rep(TRUE, nrow(cards))
  } else {
    rotated <- rotation_cycle$cycle_code[rotation_cycle$rotation_code == rotation$code]
    !(cycle_of_card %in% rotated)
  }

  if (is.null(mwl)) {
    cards$is_banned <- rep(FALSE, nrow(cards))
    cards$is_restricted <- rep(FALSE, nrow(cards))
    cards$influence_penalty <- rep(0L, nrow(cards))
    return(cards)
  }

  entries <- mwl_card[mwl_card$mwl_code == mwl$code, , drop = FALSE]
  idx <- match(cards$code, entries$card_code)

  pick <- function(column, absent) {
    value <- entries[[column]][idx]
    value[is.na(idx) | is.na(value)] <- absent
    value
  }

  cards$is_banned <- pick("deck_limit", NA_integer_) == 0L
  cards$is_banned[is.na(cards$is_banned)] <- FALSE
  cards$is_restricted <- pick("is_restricted", 0L) == 1L
  cards$influence_penalty <- as.integer(pick("universal_faction_cost", 0L))
  cards
}

#' Keep only cards legal in the given format
#'
#' "Legal" here means in rotation and not banned. Restricted cards and
#' influence-penalised cards remain legal -- those are deckbuilding
#' constraints, not exclusions -- so they are kept, with their status
#' still readable from the columns [annotate_legality()] added.
#'
#' @param cards A data frame already through [annotate_legality()].
#' @return `cards`, filtered.
#' @export
filter_legal <- function(cards) {
  cards[cards$in_rotation & !cards$is_banned, , drop = FALSE]
}

#' Search-registry fields for the legality columns
#'
#' Merged into [cardpool_search_fields()] by the app once cards have been
#' through [annotate_legality()], so `is_banned:true` and friends work in
#' the same query language as everything else.
#' @return A named list of field specs.
#' @export
legality_search_fields <- function() {
  list(
    in_rotation = new_search_field("in_rotation", "boolean", aliases = c("rotated_in")),
    is_banned = new_search_field("is_banned", "boolean", aliases = c("banned")),
    is_restricted = new_search_field("is_restricted", "boolean", aliases = c("restricted")),
    influence_penalty = new_search_field("influence_penalty", "integer")
  )
}
