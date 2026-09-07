#' Merge abr and cobra tournament records into one abr-shaped feed
#'
#' Applies the human-verified override log
#' (`read_abr_cobra_verified_deletes()`, `read_abr_cobra_verified_group_matches()`,
#' `read_abr_cobra_verified_matches()` -- R/abr-cobra-xref.R) before the
#' algorithmic tier runs, exactly the precedence those readers' own
#' docstrings require: deletes remove a cobra row from consideration
#' entirely, group matches union several cobra rows into one abr-recorded
#' event, verified 1:1 matches are forced, and only the ids no earlier
#' tier claimed reach the algorithmic tier. Anything left over on either
#' side becomes its own single-source row. (DL-059)
#'
#' Returns two frames rather than one: `tournament_merged` keeps
#' `abr.tournament`'s exact column set (so the app's read path is a
#' straight `query_active_release()` swap with no reshape), and
#' `tournament_merged_source` is a one-to-many provenance table, because a
#' group match can have several contributing cobra rows under one merged
#' id -- a flat `abr_id`/`cobra_tournament_id` column pair on the first
#' table cannot represent that. (DL-049, DL-050)
#'
#' @param abr_tournament A data frame shaped like `abr.tournament`
#'   (id, title, date, format, type, location_state, location_country,
#'   location_lat, location_lng, players_count, top_count,
#'   winner_runner_identity, winner_corp_identity).
#' @param cobra_tournament A data frame shaped like `cobra.tournament`
#'   (must include at least tournament_id, name, date).
#' @param cobra_stage A data frame shaped like `cobra.stage` (must include
#'   tournament_id, player_count).
#' @param cobra_standing A data frame shaped like `cobra.standing` (must
#'   include tournament_id, stage_format, rounds_complete, position,
#'   corp_identity, runner_identity).
#' @param cardpool_titles A data frame with `code`/`title` columns, as
#'   returned by `cardpool_card_titles()`.
#' @param verified_matches,verified_deletes,verified_groups Override
#'   readers, injectable for tests; default to the packaged CSVs via
#'   `R/abr-cobra-xref.R`.
#'
#' @return A list with `tournament_merged` and `tournament_merged_source`.
#' @keywords internal
merge_abr_cobra <- function(abr_tournament, cobra_tournament, cobra_stage, cobra_standing, cardpool_titles,
                             verified_matches = read_abr_cobra_verified_matches(),
                             verified_deletes = read_abr_cobra_verified_deletes(),
                             verified_groups = read_abr_cobra_verified_group_matches()) {
  deleted_cobra_ids <- verified_deletes$source_id[verified_deletes$source == "cobra"]
  cobra_tournament <- cobra_tournament[!cobra_tournament$tournament_id %in% deleted_cobra_ids, , drop = FALSE]
  cobra_stage <- cobra_stage[!cobra_stage$tournament_id %in% deleted_cobra_ids, , drop = FALSE]
  cobra_standing <- cobra_standing[!cobra_standing$tournament_id %in% deleted_cobra_ids, , drop = FALSE]

  facts <- cobra_tournament_facts(cobra_stage, cobra_standing)
  cobra <- merge(cobra_tournament, facts, by = "tournament_id", all.x = TRUE)
  cobra$winner_corp_identity <- resolve_cobra_identity_code(cobra$winner_corp_identity_name, cardpool_titles)
  cobra$winner_runner_identity <- resolve_cobra_identity_code(cobra$winner_runner_identity_name, cardpool_titles)

  abr <- abr_tournament

  merged <- list()
  sources <- list()
  used_cobra <- character(0)
  used_abr <- character(0)

  add_pairing <- function(abr_ids, cobra_ids, tier) {
    abr_ids <- as.character(abr_ids)
    cobra_ids <- as.character(cobra_ids)
    abr_rows <- abr[abr$id %in% abr_ids, , drop = FALSE]
    cobra_rows <- cobra[cobra$tournament_id %in% cobra_ids, , drop = FALSE]

    merged_id <- paste(c(
      if (length(abr_ids) > 0) paste0("abr:", abr_ids) else character(0),
      if (length(cobra_ids) > 0) paste0("cobra:", sort(cobra_ids)) else character(0)
    ), collapse = "+")

    merged[[length(merged) + 1L]] <<- merge_survivorship(merged_id, abr_rows, cobra_rows)
    if (length(abr_ids) > 0) {
      sources[[length(sources) + 1L]] <<- data.frame(
        merged_id = merged_id, source = "abr", source_id = abr_ids, match_tier = tier,
        stringsAsFactors = FALSE
      )
    }
    if (length(cobra_ids) > 0) {
      sources[[length(sources) + 1L]] <<- data.frame(
        merged_id = merged_id, source = "cobra", source_id = cobra_ids, match_tier = tier,
        stringsAsFactors = FALSE
      )
    }
    used_abr <<- c(used_abr, abr_ids)
    used_cobra <<- c(used_cobra, cobra_ids)
  }

  # Tier 1: verified group matches (many cobra rows -> one abr row).
  for (aid in unique(verified_groups$abr_id)) {
    cids <- verified_groups$cobra_tournament_id[verified_groups$abr_id == aid]
    cids <- intersect(cids, cobra$tournament_id)
    if (length(cids) == 0) next
    add_pairing(aid, cids, "group")
  }

  # Tier 2: verified 1:1 matches.
  for (i in seq_len(nrow(verified_matches))) {
    aid <- verified_matches$abr_id[i]
    cid <- verified_matches$cobra_tournament_id[i]
    if (!(aid %in% abr$id) || !(cid %in% cobra$tournament_id)) next
    add_pairing(aid, cid, "verified")
  }

  # Tier 3: algorithmic, over whatever neither override tier claimed.
  remaining_cobra <- cobra[!cobra$tournament_id %in% used_cobra, , drop = FALSE]
  remaining_abr <- abr[!abr$id %in% used_abr, , drop = FALSE]
  algo <- abr_cobra_algorithmic_matches(remaining_cobra, remaining_abr)
  for (i in seq_len(nrow(algo))) {
    add_pairing(algo$id[i], algo$tournament_id[i], "algorithmic")
  }

  # Tier 4: whatever's left on either side is its own single-source row.
  for (aid in abr$id[!abr$id %in% used_abr]) add_pairing(aid, character(0), "single")
  for (cid in cobra$tournament_id[!cobra$tournament_id %in% used_cobra]) add_pairing(character(0), cid, "single")

  list(
    tournament_merged = dplyr::bind_rows(merged),
    tournament_merged_source = dplyr::bind_rows(sources)
  )
}

#' Fill a merged tournament row's columns from their preferred source
#'
#' abr wins title/date/format/type/location_* whenever an abr row
#' contributes -- cobra has no location column at all
#' (`COBRA_DENY_PATTERN` rejects lat/lon, and `cobra.sql` defines no
#' location field) and no curated `type` taxonomy equivalent to abr's.
#' `players_count` prefers cobra's stage-level figure (summed across a
#' group's cobra rows) when it is known and non-zero, else abr's scalar.
#' The two winner-identity columns prefer cobra's resolved code when one
#' exists, else abr's -- cobra's is recomputed from actual per-round
#' standings (`cobra_tournament_facts()`), abr's is a flat, uncheckable
#' scalar with no per-result data anywhere in that mirror to derive or
#' contradict it (DL-051, DL-066). `top_count` is abr-only; passthrough.
#' @keywords internal
merge_survivorship <- function(merged_id, abr_rows, cobra_rows) {
  has_abr <- nrow(abr_rows) > 0
  has_cobra <- nrow(cobra_rows) > 0
  abr_row <- if (has_abr) abr_rows[1, ] else NULL

  cobra_players_count <- NA_integer_
  if (has_cobra) {
    vals <- cobra_rows$players_count
    if (!all(is.na(vals)) && !all(vals == 0, na.rm = TRUE)) {
      cobra_players_count <- as.integer(sum(vals, na.rm = TRUE))
    }
  }
  players_count <- if (!is.na(cobra_players_count) && cobra_players_count > 0) {
    cobra_players_count
  } else if (has_abr) {
    as.integer(abr_row$players_count)
  } else {
    NA_integer_
  }

  cobra_corp <- if (has_cobra) cobra_rows$winner_corp_identity[1] else NA_character_
  cobra_runner <- if (has_cobra) cobra_rows$winner_runner_identity[1] else NA_character_
  winner_corp <- if (!is.na(cobra_corp)) cobra_corp else if (has_abr) abr_row$winner_corp_identity else NA_character_
  winner_runner <- if (!is.na(cobra_runner)) cobra_runner else if (has_abr) abr_row$winner_runner_identity else NA_character_

  data.frame(
    id = merged_id,
    title = if (has_abr) abr_row$title else NA_character_,
    date = if (has_abr) abr_row$date else cobra_rows$date[1],
    format = if (has_abr) abr_row$format else NA_character_,
    type = if (has_abr) abr_row$type else NA_character_,
    location_state = if (has_abr) abr_row$location_state else NA_character_,
    location_country = if (has_abr) abr_row$location_country else NA_character_,
    location_lat = if (has_abr) as.numeric(abr_row$location_lat) else NA_real_,
    location_lng = if (has_abr) as.numeric(abr_row$location_lng) else NA_real_,
    players_count = players_count,
    top_count = if (has_abr) as.integer(abr_row$top_count) else NA_integer_,
    winner_runner_identity = winner_runner,
    winner_corp_identity = winner_corp,
    stringsAsFactors = FALSE
  )
}

#' Derive per-cobra-tournament facts that live outside `cobra.tournament`
#'
#' `players_count` is `MAX(stage.player_count)` across a tournament's
#' stages (cobra tracks participation per stage, not on the tournament
#' row). Winner identity comes from the `position == 1` standing row,
#' preferring a complete elimination stage: `standing` carries no
#' `stage_id`, so it cannot be joined to `stage.is_elimination` --
#' `stage_format %in% c("double_elim", "single_elim")` is the schema's
#' own stand-in for that flag, confirmed against the live release to
#' cover exactly the same rows `stage.is_elimination = 1` does (DL-064).
#' A tournament can have more than one `position == 1` row across its
#' stages (417 such tournaments in the live release) -- this always
#' resolves to exactly one, never by input order: an elimination-stage
#' row with `rounds_complete > 0` if any exists, else the swiss-stage
#' row, and ties within that broken by ascending `player_id` for
#' determinism across builds.
#' @keywords internal
cobra_tournament_facts <- function(cobra_stage, cobra_standing) {
  if (nrow(cobra_stage) == 0) {
    player_counts <- data.frame(tournament_id = character(0), players_count = integer(0))
  } else {
    player_counts <- stats::aggregate(
      player_count ~ tournament_id, data = cobra_stage,
      FUN = function(x) suppressWarnings(max(x, na.rm = TRUE))
    )
    names(player_counts)[names(player_counts) == "player_count"] <- "players_count"
    player_counts$players_count[is.infinite(player_counts$players_count)] <- NA_integer_
  }

  elimination_formats <- c("double_elim", "single_elim")
  swiss_formats <- c("swiss", "single_sided_swiss")

  elim_winners <- cobra_standing[
    cobra_standing$stage_format %in% elimination_formats &
      !is.na(cobra_standing$rounds_complete) & cobra_standing$rounds_complete > 0 &
      cobra_standing$position == 1,
  ]
  tournaments_with_elim_winner <- unique(elim_winners$tournament_id)

  swiss_winners <- cobra_standing[
    cobra_standing$stage_format %in% swiss_formats &
      cobra_standing$position == 1 &
      !cobra_standing$tournament_id %in% tournaments_with_elim_winner,
  ]

  winners <- rbind(elim_winners, swiss_winners)
  if (nrow(winners) > 0) {
    winners <- winners[order(winners$tournament_id, winners$player_id), ]
    winners <- winners[!duplicated(winners$tournament_id), ]
    winners <- data.frame(
      tournament_id = winners$tournament_id,
      winner_corp_identity_name = winners$corp_identity,
      winner_runner_identity_name = winners$runner_identity,
      stringsAsFactors = FALSE
    )
  } else {
    winners <- data.frame(
      tournament_id = character(0), winner_corp_identity_name = character(0),
      winner_runner_identity_name = character(0), stringsAsFactors = FALSE
    )
  }

  merge(player_counts, winners, by = "tournament_id", all = TRUE)
}

#' Resolve a cobra identity name to abr's numeric card code
#'
#' The lookup is many-to-one: an identity title reprinted across sets has
#' several codes (e.g. "NBN: Making News" has three in the live cardpool
#' release). Ties break on the lexicographically smallest code, so the
#' result is deterministic across builds rather than dependent on input
#' row order. This is sound only as a **faction key**, not a claim about
#' which printing was played: no identity title in the cardpool release
#' spans more than one `faction_code`, and the only readers of this
#' column (`views-meta-stats.R`, `views-meta-trends.R`) use it purely to
#' look up a faction. A future view that renders the winning card image
#' must resolve the printing itself rather than trust this code.
#' (DL-052, DL-065)
#'
#' @param names Character vector of cobra identity names (may contain NA).
#' @param cardpool_titles A data frame with `code`/`title` columns.
#' @return Character vector of resolved codes, `NA` where `names` is `NA`
#'   or has no matching title.
#' @keywords internal
resolve_cobra_identity_code <- function(names, cardpool_titles) {
  vapply(names, function(nm) {
    if (is.na(nm)) return(NA_character_)
    codes <- cardpool_titles$code[cardpool_titles$title == nm]
    if (length(codes) == 0) return(NA_character_)
    min(codes)
  }, character(1), USE.NAMES = FALSE)
}

#' Find abr/cobra tournament pairs by date, name similarity and player count
#'
#' Blocks candidate pairs on date within -1/0/+1 days (cobra's date is
#' already ISO; abr's upstream format is `"YYYY.MM.DD."`), requires
#' normalized-name similarity >= 0.85, requires player counts within +/-1
#' (or either side missing), and rejects any pair where either side has
#' more than one surviving candidate -- the same conservative rule
#' validated against live production data (913 confirmed matches) before
#' this was ever committed as package code.
#'
#' Name similarity is computed with
#' `mapply(function(x, y) utils::adist(x, y)[1, 1], a, b)`, never by
#' indexing a full `adist(a, b)` matrix: `adist()` builds the entire
#' `length(a) x length(b)` distance matrix before a `[cbind(...)]` index
#' discards all but the diagonal, which is O(N^2) rather than O(N) --
#' confirmed to burn 18+ CPU-minutes on ~17k rows in a real incident
#' before being killed. (DL-058)
#'
#' @param cobra_tournament A data frame with tournament_id, name, date,
#'   players_count.
#' @param abr_tournament A data frame with id, title, date, players_count.
#' @return A data frame with one row per matched (tournament_id, id) pair.
#' @keywords internal
abr_cobra_algorithmic_matches <- function(cobra_tournament, abr_tournament) {
  if (nrow(cobra_tournament) == 0 || nrow(abr_tournament) == 0) {
    return(data.frame(tournament_id = character(0), id = character(0)))
  }

  norm_name <- function(x) {
    x <- tolower(x)
    x <- gsub("[^a-z ]", " ", x)
    trimws(gsub("\\s+", " ", x))
  }

  ct <- cobra_tournament
  ct$name_norm <- norm_name(ct$name)
  ct$cobra_date <- as.Date(ct$date)

  at <- abr_tournament
  at$title_norm <- norm_name(at$title)
  at$abr_date <- as.Date(at$date, format = "%Y.%m.%d.")

  candidates <- lapply(c(-1L, 0L, 1L), function(offset) {
    ct$join_date <- ct$cobra_date + offset
    merge(ct, at, by.x = "join_date", by.y = "abr_date")
  })
  cand <- dplyr::bind_rows(candidates)
  if (nrow(cand) == 0) {
    return(data.frame(tournament_id = character(0), id = character(0)))
  }

  cand$sim <- 1 - (
    mapply(function(a, b) utils::adist(a, b)[1, 1], cand$name_norm, cand$title_norm) /
      pmax(nchar(cand$name_norm), nchar(cand$title_norm), 1)
  )
  cand$pc_ok <- is.na(cand$players_count.x) | is.na(cand$players_count.y) |
    abs(cand$players_count.x - cand$players_count.y) <= 1

  cand <- cand[cand$sim >= 0.85 & cand$pc_ok, ]
  cobra_counts <- table(cand$tournament_id)
  abr_counts <- table(cand$id)
  unique_matches <- cand[
    cand$tournament_id %in% names(cobra_counts[cobra_counts == 1]) &
      cand$id %in% names(abr_counts[abr_counts == 1]),
  ]

  unique(unique_matches[, c("tournament_id", "id")])
}
