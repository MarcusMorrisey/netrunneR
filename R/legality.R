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
#' @param format Character. A real format id, now that `mwl.format` is
#'   joined from the v2 restriction table rather than guessed from a code
#'   prefix -- `"standard"` (the default), `"startup"`, `"eternal"` or
#'   `"snapshot"`. There is no `"napd"` or `"sunset"` format; those
#'   prefixes name Standard lists. Prefer [active_snapshot()], which also
#'   resolves the card pool.
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
    influence_penalty = new_search_field("influence_penalty", "integer"),
    # Only annotate_format_legality() produces these two; a card set
    # annotated by the legacy path simply has no such column, and the
    # evaluator reports an unknown field rather than matching nothing.
    global_penalty = new_search_field("global_penalty", "integer"),
    points = new_search_field("points", "integer")
  )
}

# ---- Format snapshots (the authoritative path) --------------------------
#
# active_rotation() / active_mwl() above read the legacy rotations.json
# and mwl.json tables. The functions below read the v2 format /
# format_snapshot / card_pool / restriction tables, which state outright
# what those two only imply: which format a ban list belongs to, and
# which sets are in a card pool. Prefer these; the legacy pair is kept
# because the rotation table has no v2 equivalent and callers still use it.

#' The format snapshot in force on a given date
#'
#' Selection is by date, deliberately, and NOT by the `is_active` column.
#' `is_active` mirrors upstream's own hand-maintained `active` flag,
#' which lags: at the mirrored commit it still marked the March 2026
#' Standard snapshot active although two later ones had already started.
#' The flag is carried so the build's snapshot_active_flag check can
#' report the disagreement, not so it can drive this choice.
#'
#' @param format_snapshot The cardpool `format_snapshot` table.
#' @param format Character. A format id -- `"standard"` (the default),
#'   `"startup"`, `"eternal"`, `"snapshot"`, `"core"`,
#'   `"system_gateway"` or `"ram"`.
#' @param as_of Date. Defaults to today.
#' @return A one-row data frame, or NULL when that format has no snapshot
#'   yet in force.
#' @export
active_snapshot <- function(format_snapshot, format = "standard", as_of = Sys.Date()) {
  if (!nrow(format_snapshot)) return(NULL)
  candidates <- format_snapshot[
    format_snapshot$format_id == format & as.Date(format_snapshot$date_start) <= as_of,
    , drop = FALSE
  ]
  if (!nrow(candidates)) return(NULL)
  candidates[which.max(as.Date(candidates$date_start)), , drop = FALSE]
}

#' The card sets making up a card pool
#'
#' @param card_pool_set The cardpool `card_pool_set` table.
#' @param card_pool_id Character. The pool to resolve.
#' @return Character vector of card set ids.
#' @export
card_pool_sets <- function(card_pool_set, card_pool_id) {
  card_pool_set$card_set_id[card_pool_set$card_pool_id == card_pool_id]
}

#' Map card codes to their v2 card set
#'
#' Uses the `printing` table, falling back to the card's own `pack_code`
#' resolved through `card_set$legacy_code` for any code with no printing
#' row. The two agree 1:1 upstream, so the fallback matters only for a
#' mirrored commit predating the v2 tree or a hand-built fixture -- but
#' it is what keeps a card from silently dropping out of every pool.
#' @param codes Character vector of card codes.
#' @param pack_codes Character vector of the same length: each card's own
#'   `pack_code`. Taken from the cards themselves rather than looked up
#'   from a pack table, which is keyed by pack and cannot answer "which
#'   pack is this CARD in".
#' @param printing The cardpool `printing` table.
#' @param card_set The cardpool `card_set` table.
#' @return Character vector of card set ids, NA where unresolvable.
#' @keywords internal
card_set_of_code <- function(codes, pack_codes, printing, card_set) {
  set_id <- if (nrow(printing)) printing$card_set_id[match(codes, printing$code)] else rep(NA_character_, length(codes))
  missing <- is.na(set_id)
  if (any(missing) && nrow(card_set)) {
    set_id[missing] <- card_set$id[match(pack_codes[missing], card_set$legacy_code)]
  }
  set_id
}

#' Annotate cards against a format snapshot
#'
#' The v2 counterpart to [annotate_legality()], and the one to prefer.
#' It differs in two ways that matter:
#'
#' * Card-pool membership is read POSITIVELY from the snapshot's pool
#'   (the sets that are in) rather than inferred from a rotation's list
#'   of cycles that rotated out.
#' * The ban list is the one the snapshot names, so it is always the
#'   right list for the format -- the legacy path had to guess a list's
#'   format from its code prefix, which was wrong for 13 of 41 lists.
#'
#' The first four columns added are named exactly as [annotate_legality()]
#' names them, so [filter_legal()] and [legality_search_fields()] work
#' against either path's output. `in_rotation` here means "in this
#' snapshot's card pool".
#'
#' Two further columns carry what the legacy tables could not express:
#' `global_penalty`, and `points` for the points-list formats (Eternal),
#' where a card is not banned but costs against a deck's point limit.
#'
#' @param cards The cardpool `card` table.
#' @param snapshot A one-row snapshot from [active_snapshot()], or NULL
#'   for "no format applies", which leaves every card in pool and clean.
#' @param card_pool_set The cardpool `card_pool_set` table.
#' @param restriction_card The cardpool `restriction_card` table.
#' @param printing The cardpool `printing` table.
#' @param card_set The cardpool `card_set` table.
#' @return `cards` with six columns added.
#' @export
annotate_format_legality <- function(cards, snapshot, card_pool_set, restriction_card,
                                     printing, card_set) {
  clean <- function(df) {
    df$in_rotation <- rep(TRUE, nrow(df))
    df$is_banned <- rep(FALSE, nrow(df))
    df$is_restricted <- rep(FALSE, nrow(df))
    df$influence_penalty <- rep(0L, nrow(df))
    df$global_penalty <- rep(0L, nrow(df))
    df$points <- rep(0L, nrow(df))
    df
  }
  if (is.null(snapshot)) return(clean(cards))

  cards <- clean(cards)

  pool_sets <- card_pool_sets(card_pool_set, snapshot$card_pool_id)
  # An empty pool means "this pool lists no sets", which is not the same
  # as "no pool applies" -- keeping every card in would silently pass
  # cards no format allows.
  cards$in_rotation <- card_set_of_code(cards$code, cards$pack_code, printing, card_set) %in% pool_sets

  # A snapshot can legitimately have no restriction (core, system_gateway,
  # ram), which is a card pool with no ban list, not missing data.
  if (is.na(snapshot$restriction_id)) return(cards)

  entries <- restriction_card[restriction_card$restriction_id == snapshot$restriction_id, , drop = FALSE]
  if (!nrow(entries)) return(cards)

  card_ids <- if (nrow(printing)) printing$card_id[match(cards$code, printing$code)] else rep(NA_character_, nrow(cards))
  idx <- match(card_ids, entries$card_id)

  pick <- function(column, absent) {
    value <- entries[[column]][idx]
    value[is.na(idx) | is.na(value)] <- absent
    value
  }

  cards$is_banned <- pick("is_banned", 0L) == 1L
  cards$is_restricted <- pick("is_restricted", 0L) == 1L
  cards$influence_penalty <- as.integer(pick("universal_faction_cost", 0L))
  cards$global_penalty <- as.integer(pick("global_penalty", 0L))
  cards$points <- as.integer(pick("points", 0L))
  cards
}
