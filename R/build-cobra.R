#' Fail-closed dplyr::select() allowlists for the Cobra tables
#'
#' Cobra's pairings and standings endpoints carry real player display
#' names (player1_name/player2_name/name_with_pronouns) directly in the
#' response body -- unlike ABR, whose winner_*_identity fields name a
#' game identity, not a person. These allowlists exclude every such
#' field on the same precedent DEFAULT_DENY_PATTERN and
#' ABR_TOURNAMENT_ALLOWLIST already establish: personal data is excluded
#' by two independent fail-closed layers, never passed through and
#' relied on downstream code not to use. Layered with
#' check_deny_pattern(COBRA_DENY_PATTERN) below as an independent second
#' check, not a restatement of these allowlists. (ref: DL-002)
#'
#' ABR_CODE IS A PLAIN REFERENCE, NOT A COMPUTED MATCH. abr_code arrives
#' from Cobra's own upstream API already naming a tournament's ABR id --
#' netrunneR computes nothing here. It is therefore a plain reference to
#' abr.tournament.id in the abr lineage, joinable ad hoc by any consumer
#' that reads both releases, the way R/build-implementation.R reads
#' another lineage via query_active_release() (R/operations.R).
#'
#' Two properties are unverified. Upstream does not populate abr_code for
#' every tournament, and its actual fill rate is unmeasured -- cobra has
#' no live data as of this writing, so no figure or proportion is given
#' here. Whether abr_code's string values compare directly against
#' abr.tournament.id after cast and trim is also unconfirmed, so a bare
#' equality join is not a checked operation.
#'
#' No cross-reference table exists, and none is built here: a built
#' cross-reference is gated on cobra carrying live data and a named
#' consumer needing the combined read. (ref: DL-040, DL-041)
#' @keywords internal
COBRA_TOURNAMENT_ALLOWLIST <- c(
  "tournament_id", "name", "slug", "abr_code", "private", "date", "time_zone",
  "registration_starts", "tournament_starts", "decklist_required", "self_registration",
  "allow_self_reporting", "swiss_deck_visibility", "cut_deck_visibility", "swiss_format",
  "tournament_type_id", "format_id", "deckbuilding_restriction_id", "card_set_id",
  "created_at", "updated_at", "policy_update", "custom_table_numbering",
  "stage_count", "round_count", "pairing_count"
)

#' @keywords internal
COBRA_STAGE_ALLOWLIST <- c(
  "tournament_id", "stage_id", "stage_name", "format", "is_single_sided",
  "is_elimination", "view_decks", "player_count"
)

#' @keywords internal
COBRA_ROUND_ALLOWLIST <- c(
  "tournament_id", "stage_id", "stage_name", "stage_format", "round_id", "round_number",
  "completed", "pairings_reported", "length_minutes", "timer_running", "timer_paused",
  "timer_started", "pairing_count"
)

#' @keywords internal
COBRA_PAIRING_ALLOWLIST <- c(
  "tournament_id", "stage_id", "stage_name", "stage_format", "round_id", "round_number",
  "pairing_id", "table_number", "table_label", "reported", "intentional_draw", "two_for_one",
  "score1", "score2", "score_label",
  "player1_id", "player1_seed", "player1_side", "player1_corp_identity", "player1_corp_faction",
  "player1_runner_identity", "player1_runner_faction",
  "player2_id", "player2_seed", "player2_side", "player2_corp_identity", "player2_corp_faction",
  "player2_runner_identity", "player2_runner_faction"
)

#' @keywords internal
COBRA_STANDING_ALLOWLIST <- c(
  "tournament_id", "stage_format", "rounds_complete", "any_decks_viewable",
  "position", "player_id", "active", "corp_identity", "corp_faction",
  "runner_identity", "runner_faction", "points", "sos", "extended_sos",
  "corp_points", "runner_points", "bye_points", "seed", "manual_seed",
  "side_bias", "view_decks"
)

#' @keywords internal
COBRA_FACTION_COUNT_ALLOWLIST <- c("tournament_id", "scope", "side", "faction", "count", "num_players")

#' @keywords internal
COBRA_IDENTITY_COUNT_ALLOWLIST <- c(
  "tournament_id", "scope", "side", "identity_name", "faction", "count", "num_players"
)

#' @keywords internal
COBRA_CUT_CONVERSION_FACTION_ALLOWLIST <- c(
  "tournament_id", "side", "faction", "num_swiss_players", "num_cut_players", "cut_conversion_percentage"
)

#' @keywords internal
COBRA_CUT_CONVERSION_IDENTITY_ALLOWLIST <- c(
  "tournament_id", "side", "identity_name", "faction", "num_swiss_players", "num_cut_players",
  "cut_conversion_percentage"
)

#' @keywords internal
COBRA_RECENT_INDEX_ALLOWLIST <- c("tournament_id", "tournament_type_id", "discovered_at")

#' Cobra-specific deny pattern for personal-data column names
#'
#' A specialization of DEFAULT_DENY_PATTERN (R/validate-helpers.R) naming
#' the exact upstream field shapes this lineage must exclude: player
#' display names (player1_name, player2_name, ...), the
#' name_with_pronouns field, and the same organizer/contact/address
#' families ABR_DENY_PATTERN (R/build-abr.R) already covers. Run as an
#' independent second enforcement layer separate from and after the
#' dplyr::select() allowlists in build_cobra(), never a restatement of
#' them. (ref: DL-002)
#'
#' Unlike ABR_DENY_PATTERN, this pattern still rejects coordinates:
#' Cobra's tournament records carry no venue-coordinate field at all, so
#' there is no equivalent of the ABR_TOURNAMENT_ALLOWLIST reversal to
#' make here.
#' @keywords internal
COBRA_DENY_PATTERN <- "(?i)(contact|e[-_]?mail|player[0-9]*[-_]?(name|handle)|user[-_]?(name|handle)|pronoun|creator|importer|uploader|organizer|address|lat(itude)?|lon(gitude)?|\\bbio\\b|notes?|description)"

#' Build the cobra release
#'
#' Flattens every resolved bundle in the pool (R/fetch-cobra.R) into the
#' ten Cobra tables, applying an all_of() allowlist select() per table
#' (fails closed on a shrunk or mistyped allowlist, matching
#' ABR_TOURNAMENT_ALLOWLIST's discipline) and then an independent
#' check_deny_pattern(COBRA_DENY_PATTERN) scan across every table before
#' any write.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "cobra".
#' @param staged_raw The value returned by fetch_cobra().
#'
#' @keywords internal
build_cobra <- function(lineage, staged_raw) {
  bundles <- staged_raw$bundles

  tournaments <- cobra_bind_allowlisted(lapply(bundles, flatten_cobra_tournament), COBRA_TOURNAMENT_ALLOWLIST)
  stages <- cobra_bind_allowlisted(lapply(bundles, flatten_cobra_stages), COBRA_STAGE_ALLOWLIST)
  rounds <- cobra_bind_allowlisted(lapply(bundles, flatten_cobra_rounds), COBRA_ROUND_ALLOWLIST)
  pairings <- cobra_bind_allowlisted(lapply(bundles, flatten_cobra_pairings), COBRA_PAIRING_ALLOWLIST)
  standings <- cobra_bind_allowlisted(lapply(bundles, flatten_cobra_standings), COBRA_STANDING_ALLOWLIST)

  id_faction_parts <- lapply(bundles, function(b) flatten_cobra_id_and_faction(b$id_and_faction_data, b$tournament_id))
  faction_counts <- cobra_bind_allowlisted(lapply(id_faction_parts, `[[`, "faction_counts"), COBRA_FACTION_COUNT_ALLOWLIST)
  identity_counts <- cobra_bind_allowlisted(lapply(id_faction_parts, `[[`, "identity_counts"), COBRA_IDENTITY_COUNT_ALLOWLIST)

  conversion_parts <- lapply(bundles, function(b) flatten_cobra_cut_conversion(b$cut_conversion_rates, b$tournament_id))
  cut_conversion_factions <- cobra_bind_allowlisted(
    lapply(conversion_parts, `[[`, "cut_conversion_factions"), COBRA_CUT_CONVERSION_FACTION_ALLOWLIST
  )
  cut_conversion_identities <- cobra_bind_allowlisted(
    lapply(conversion_parts, `[[`, "cut_conversion_identities"), COBRA_CUT_CONVERSION_IDENTITY_ALLOWLIST
  )

  recent_index <- cobra_bind_allowlisted(list(staged_raw$recent_index), COBRA_RECENT_INDEX_ALLOWLIST)

  all_tables <- list(
    tournament = tournaments, stage = stages, round = rounds, pairing = pairings,
    standing = standings, faction_count = faction_counts, identity_count = identity_counts,
    cut_conversion_faction = cut_conversion_factions, cut_conversion_identity = cut_conversion_identities,
    recent_index = recent_index
  )

  # Layer two: an independent regex scan of column names across every
  # table, run after the allowlist select()s above rather than instead
  # of them -- see COBRA_DENY_PATTERN's docstring for why both layers
  # are required.
  deny_checks <- lapply(names(all_tables), function(nm) {
    result <- check_deny_pattern(all_tables[[nm]], COBRA_DENY_PATTERN)
    result$check <- sprintf("%s:%s", result$check, nm)
    result
  })
  failed_deny <- Filter(function(x) identical(x$status, "fail"), deny_checks)
  if (length(failed_deny) > 0) {
    rlang::abort(failed_deny[[1]]$message, class = "netrunneR_deny_pattern_violation")
  }

  processed_dir <- file.path(dirname(staged_raw$raw_dir), "processed")
  fs::dir_create(processed_dir, mode = "2750")
  db_path <- file.path(processed_dir, "cobra.sqlite")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  apply_schema(con, "cobra")
  DBI::dbWithTransaction(con, {
    for (nm in names(all_tables)) {
      DBI::dbWriteTable(con, nm, all_tables[[nm]], append = TRUE)
    }
  })
  Sys.chmod(db_path, mode = "0640")

  br <- build_revision(lineage, build_module_path = "R/build-cobra.R")
  # build_revision is computed identically for every lineage, cobra
  # included -- this call is not a narrowed or lineage-specific variant.

  rows_check <- list(
    check = "tournament_row_count",
    status = "pass",
    message = sprintf("%d tournament row(s) built from %d pool bundle(s)", nrow(tournaments), length(bundles))
  )

  list(
    db_path = db_path,
    build_revision = br,
    release_id = sprintf("%s-%s", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), release_entropy_suffix()),
    checks = c(staged_raw$checks %||% list(), deny_checks, list(rows_check))
  )
}

#' Bind a list of per-tournament data frames (any NULL or empty entries
#' dropped) and select only the allowlisted columns, erroring closed via
#' all_of() if the allowlist names a column the flattened frame is
#' missing.
#' @keywords internal
cobra_bind_allowlisted <- function(items, allowlist) {
  items <- Filter(function(x) !is.null(x) && NROW(x) > 0, items)
  if (length(items) == 0) {
    return(tibble::as_tibble(stats::setNames(rep(list(character(0)), length(allowlist)), allowlist)))
  }
  bound <- dplyr::bind_rows(items)
  dplyr::select(bound, dplyr::all_of(allowlist))
}

#' @keywords internal
flatten_cobra_tournament <- function(bundle) {
  obj <- bundle$pairings_data
  tournament <- obj$tournament
  stages <- obj$stages %||% list()
  round_count <- sum(vapply(stages, function(stage) length(stage$rounds %||% list()), integer(1)))
  pairing_count <- sum(vapply(stages, function(stage) {
    sum(vapply(stage$rounds %||% list(), function(round) length(round$pairings %||% list()), integer(1)))
  }, integer(1)))

  tibble::tibble(
    tournament_id = as.character(tournament$id %||% NA_integer_),
    name = tournament$name %||% NA_character_,
    slug = tournament$slug %||% NA_character_,
    abr_code = tournament$abr_code %||% NA_character_,
    private = as.logical(tournament$private %||% NA),
    date = tournament$date %||% NA_character_,
    time_zone = tournament$time_zone %||% NA_character_,
    registration_starts = tournament$registration_starts %||% NA_character_,
    tournament_starts = tournament$tournament_starts %||% NA_character_,
    decklist_required = as.logical(tournament$decklist_required %||% NA),
    self_registration = as.logical(tournament$self_registration %||% NA),
    allow_self_reporting = as.logical(tournament$allow_self_reporting %||% NA),
    swiss_deck_visibility = tournament$swiss_deck_visibility %||% NA_character_,
    cut_deck_visibility = tournament$cut_deck_visibility %||% NA_character_,
    swiss_format = tournament$swiss_format %||% NA_character_,
    tournament_type_id = as.integer(tournament$tournament_type_id %||% NA_integer_),
    format_id = as.integer(tournament$format_id %||% NA_integer_),
    deckbuilding_restriction_id = tournament$deckbuilding_restriction_id %||% NA_character_,
    card_set_id = tournament$card_set_id %||% NA_character_,
    created_at = tournament$created_at %||% NA_character_,
    updated_at = tournament$updated_at %||% NA_character_,
    policy_update = as.logical(obj$policy$update %||% NA),
    custom_table_numbering = as.logical(obj$policy$custom_table_numbering %||% NA),
    stage_count = length(stages),
    round_count = round_count,
    pairing_count = pairing_count
  )
}

#' @keywords internal
flatten_cobra_stages <- function(bundle) {
  obj <- bundle$pairings_data
  stages <- obj$stages %||% list()
  if (length(stages) == 0) return(NULL)

  rows <- lapply(stages, function(stage) {
    tibble::tibble(
      tournament_id = as.character(obj$tournament$id %||% NA_integer_),
      stage_id = as.integer(stage$id %||% NA_integer_),
      stage_name = stage$name %||% NA_character_,
      format = stage$format %||% NA_character_,
      is_single_sided = as.logical(stage$is_single_sided %||% NA),
      is_elimination = as.logical(stage$is_elimination %||% NA),
      view_decks = as.logical(stage$view_decks %||% NA),
      player_count = as.integer(stage$player_count %||% NA_integer_)
    )
  })
  dplyr::bind_rows(rows)
}

#' @keywords internal
flatten_cobra_rounds <- function(bundle) {
  obj <- bundle$pairings_data
  stages <- obj$stages %||% list()
  rows <- list()

  for (stage in stages) {
    rounds <- stage$rounds %||% list()
    for (round in rounds) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        tournament_id = as.character(obj$tournament$id %||% NA_integer_),
        stage_id = as.integer(stage$id %||% NA_integer_),
        stage_name = stage$name %||% NA_character_,
        stage_format = stage$format %||% NA_character_,
        round_id = as.integer(round$id %||% NA_integer_),
        round_number = as.integer(round$number %||% NA_integer_),
        completed = as.logical(round$completed %||% NA),
        pairings_reported = as.integer(round$pairings_reported %||% NA_integer_),
        length_minutes = as.integer(round$length_minutes %||% NA_integer_),
        timer_running = as.logical(round$timer$running %||% NA),
        timer_paused = as.logical(round$timer$paused %||% NA),
        timer_started = as.logical(round$timer$started %||% NA),
        pairing_count = length(round$pairings %||% list())
      )
    }
  }

  if (length(rows) == 0) NULL else dplyr::bind_rows(rows)
}

#' @keywords internal
flatten_cobra_pairings <- function(bundle) {
  obj <- bundle$pairings_data
  stages <- obj$stages %||% list()
  rows <- list()

  for (stage in stages) {
    for (round in stage$rounds %||% list()) {
      for (pairing in round$pairings %||% list()) {
        rows[[length(rows) + 1L]] <- tibble::tibble(
          tournament_id = as.character(obj$tournament$id %||% NA_integer_),
          stage_id = as.integer(stage$id %||% NA_integer_),
          stage_name = stage$name %||% NA_character_,
          stage_format = stage$format %||% NA_character_,
          round_id = as.integer(round$id %||% NA_integer_),
          round_number = as.integer(round$number %||% NA_integer_),
          pairing_id = as.integer(pairing$id %||% NA_integer_),
          table_number = as.integer(pairing$table_number %||% NA_integer_),
          table_label = pairing$table_label %||% NA_character_,
          reported = as.logical(pairing$reported %||% NA),
          intentional_draw = as.logical(pairing$intentional_draw %||% NA),
          two_for_one = as.logical(pairing$two_for_one %||% NA),
          score1 = as.numeric(pairing$score1 %||% NA_real_),
          score2 = as.numeric(pairing$score2 %||% NA_real_),
          score_label = pairing$score_label %||% NA_character_,
          player1_id = as.integer(pairing$player1$id %||% NA_integer_),
          player1_seed = as.integer(pairing$player1$seed %||% NA_integer_),
          player1_side = pairing$player1$side %||% NA_character_,
          player1_corp_identity = pairing$player1$corp_id$name %||% NA_character_,
          player1_corp_faction = pairing$player1$corp_id$faction %||% NA_character_,
          player1_runner_identity = pairing$player1$runner_id$name %||% NA_character_,
          player1_runner_faction = pairing$player1$runner_id$faction %||% NA_character_,
          player2_id = as.integer(pairing$player2$id %||% NA_integer_),
          player2_seed = as.integer(pairing$player2$seed %||% NA_integer_),
          player2_side = pairing$player2$side %||% NA_character_,
          player2_corp_identity = pairing$player2$corp_id$name %||% NA_character_,
          player2_corp_faction = pairing$player2$corp_id$faction %||% NA_character_,
          player2_runner_identity = pairing$player2$runner_id$name %||% NA_character_,
          player2_runner_faction = pairing$player2$runner_id$faction %||% NA_character_
        )
      }
    }
  }

  if (length(rows) == 0) NULL else dplyr::bind_rows(rows)
}

#' @keywords internal
flatten_cobra_standings <- function(bundle) {
  obj <- bundle$standings_data
  if (is.null(obj) || length(obj$stages %||% list()) == 0) return(NULL)

  rows <- list()
  for (stage in obj$stages %||% list()) {
    for (standing in stage$standings %||% list()) {
      rows[[length(rows) + 1L]] <- tibble::tibble(
        tournament_id = as.character(obj$tournament_id %||% bundle$tournament_id %||% NA_integer_),
        stage_format = stage$format %||% NA_character_,
        rounds_complete = as.integer(stage$rounds_complete %||% NA_integer_),
        any_decks_viewable = as.logical(stage$any_decks_viewable %||% NA),
        position = as.integer(standing$position %||% NA_integer_),
        player_id = as.integer(standing$player$id %||% NA_integer_),
        active = as.logical(standing$player$active %||% NA),
        corp_identity = standing$player$corp_id$name %||% NA_character_,
        corp_faction = standing$player$corp_id$faction %||% NA_character_,
        runner_identity = standing$player$runner_id$name %||% NA_character_,
        runner_faction = standing$player$runner_id$faction %||% NA_character_,
        points = as.numeric(standing$points %||% NA_real_),
        sos = as.numeric(standing$sos %||% NA_real_),
        extended_sos = as.numeric(standing$extended_sos %||% NA_real_),
        corp_points = as.numeric(standing$corp_points %||% NA_real_),
        runner_points = as.numeric(standing$runner_points %||% NA_real_),
        bye_points = as.numeric(standing$bye_points %||% NA_real_),
        seed = as.integer(standing$seed %||% NA_integer_),
        manual_seed = as.integer(standing$manual_seed %||% NA_integer_),
        side_bias = as.numeric(standing$side_bias %||% NA_real_),
        view_decks = as.logical(standing$policy$view_decks %||% NA)
      )
    }
  }

  if (length(rows) == 0) NULL else dplyr::bind_rows(rows)
}

#' @keywords internal
flatten_cobra_id_and_faction <- function(obj, tournament_id) {
  if (is.null(obj)) {
    return(list(faction_counts = NULL, identity_counts = NULL))
  }

  faction_rows <- list()
  identity_rows <- list()

  append_scope <- function(scope_name, scope_obj, num_players) {
    for (side in c("corp", "runner")) {
      factions <- scope_obj[[side]]$factions %||% list()
      ids <- scope_obj[[side]]$ids %||% list()

      if (length(factions) > 0) {
        for (faction_name in names(factions)) {
          faction_rows[[length(faction_rows) + 1L]] <<- tibble::tibble(
            tournament_id = as.character(tournament_id), scope = scope_name, side = side,
            faction = faction_name, count = as.integer(factions[[faction_name]]),
            num_players = as.integer(num_players)
          )
        }
      }

      if (length(ids) > 0) {
        for (identity_name in names(ids)) {
          identity_rows[[length(identity_rows) + 1L]] <<- tibble::tibble(
            tournament_id = as.character(tournament_id), scope = scope_name, side = side,
            identity_name = identity_name, faction = ids[[identity_name]]$faction %||% NA_character_,
            count = as.integer(ids[[identity_name]]$count %||% NA_integer_),
            num_players = as.integer(num_players)
          )
        }
      }
    }
  }

  append_scope("swiss", obj, obj$num_players %||% NA_integer_)
  if (!is.null(obj$cut)) {
    append_scope("cut", obj$cut, obj$cut$num_players %||% NA_integer_)
  }

  list(
    faction_counts = if (length(faction_rows) == 0) NULL else dplyr::bind_rows(faction_rows),
    identity_counts = if (length(identity_rows) == 0) NULL else dplyr::bind_rows(identity_rows)
  )
}

#' @keywords internal
flatten_cobra_cut_conversion <- function(obj, tournament_id) {
  if (is.null(obj)) {
    return(list(cut_conversion_factions = NULL, cut_conversion_identities = NULL))
  }

  faction_rows <- list()
  identity_rows <- list()

  for (side in names(obj$factions %||% list())) {
    for (faction_name in names(obj$factions[[side]] %||% list())) {
      item <- obj$factions[[side]][[faction_name]]
      faction_rows[[length(faction_rows) + 1L]] <- tibble::tibble(
        tournament_id = as.character(tournament_id), side = side, faction = faction_name,
        num_swiss_players = as.integer(item$num_swiss_players %||% NA_integer_),
        num_cut_players = as.integer(item$num_cut_players %||% NA_integer_),
        cut_conversion_percentage = as.numeric(item$cut_conversion_percentage %||% NA_real_)
      )
    }
  }

  for (side in names(obj$identities %||% list())) {
    for (identity_name in names(obj$identities[[side]] %||% list())) {
      item <- obj$identities[[side]][[identity_name]]
      identity_rows[[length(identity_rows) + 1L]] <- tibble::tibble(
        tournament_id = as.character(tournament_id), side = side, identity_name = identity_name,
        faction = item$faction %||% NA_character_,
        num_swiss_players = as.integer(item$num_swiss_players %||% NA_integer_),
        num_cut_players = as.integer(item$num_cut_players %||% NA_integer_),
        cut_conversion_percentage = as.numeric(item$cut_conversion_percentage %||% NA_real_)
      )
    }
  }

  list(
    cut_conversion_factions = if (length(faction_rows) == 0) NULL else dplyr::bind_rows(faction_rows),
    cut_conversion_identities = if (length(identity_rows) == 0) NULL else dplyr::bind_rows(identity_rows)
  )
}
