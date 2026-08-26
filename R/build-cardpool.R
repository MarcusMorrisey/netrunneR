#' Build a git-mirror lineage's processed release
#'
#' Dispatches to the cardpool or implementation build routine by lineage
#' name, since both lineages share the netrunneR_git_mirror class and
#' fetch method but have distinct schemas and outputs.
#'
#' @param lineage A lineage object of class netrunneR_git_mirror.
#' @param staged_raw The value returned by fetch_lineage.netrunneR_git_mirror().
#' @param ... Ignored.
#'
#' @export
build_lineage.netrunneR_git_mirror <- function(lineage, staged_raw, ...) {
  if (identical(lineage$name, "cardpool")) {
    build_cardpool(lineage, staged_raw)
  } else if (identical(lineage$name, "implementation")) {
    build_implementation(lineage, staged_raw)
  } else {
    rlang::abort(sprintf("No git-mirror build method for lineage '%s'", lineage$name), class = "netrunneR_no_build_method")
  }
}

#' Named allowlists of cardpool columns permitted into the processed store
#'
#' One dplyr::select() allowlist per cardpool table, matching
#' inst/sql/schema/cardpool.sql exactly. The upstream
#' Null-Signal-Games/netrunner-cards-json JSON carries substantially more
#' fields per record (e.g. card: flavor/illustrator/quantity/uniqueness;
#' cycle/pack: rotated/size/date_release/ffg_id) than this schema stores;
#' those extra fields are dropped here rather than passed through, same
#' fail-closed allowlist discipline as ABR_TOURNAMENT_ALLOWLIST, and
#' surfaced via the unknown-key check below rather than silently ignored.
#' @keywords internal
CARDPOOL_CYCLE_ALLOWLIST <- c("code", "name", "position")

#' @keywords internal
CARDPOOL_FACTION_ALLOWLIST <- c("code", "name", "side")

#' @keywords internal
CARDPOOL_PACK_ALLOWLIST <- c("code", "name", "cycle_code", "position")

#' @keywords internal
CARDPOOL_CARD_ALLOWLIST <- c(
  "code", "title", "pack_code", "faction_code", "type_code", "side_code",
  "text", "cost", "strength", "keywords"
)

#' Flatten rotations.json into rotation / rotation_cycle tables
#'
#' Upstream shape: `[{code, name, date_start, rotated: [cycle_code, ...]}]`.
#' The `rotated` array becomes one row per (rotation, cycle) rather than a
#' list-column.
#' @param path Character. Path to rotations.json.
#' @return A list with `rotation` and `rotation_cycle` tibbles.
#' @keywords internal
read_rotations <- function(path) {
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  rotation <- tibble::tibble(
    code = vapply(parsed, function(r) as.character(r$code), character(1)),
    name = vapply(parsed, function(r) as.character(r$name), character(1)),
    date_start = vapply(parsed, function(r) as.character(r$date_start), character(1))
  )

  rotation_cycle <- purrr::map_dfr(parsed, function(r) {
    rotated <- unlist(r$rotated %||% list(), use.names = FALSE)
    if (!length(rotated)) return(tibble::tibble(rotation_code = character(0), cycle_code = character(0)))
    tibble::tibble(rotation_code = as.character(r$code), cycle_code = as.character(rotated))
  })

  list(
    rotation = dplyr::arrange(rotation, .data$code),
    rotation_cycle = dplyr::arrange(rotation_cycle, .data$rotation_code, .data$cycle_code)
  )
}

#' Flatten mwl.json into mwl / mwl_card tables
#'
#' Upstream shape: `[{code, name, date_start, cards: {card_code: {key: value}}}]`
#' where each card object carries exactly ONE of `deck_limit`,
#' `is_restricted`, `universal_faction_cost` or `global_penalty`. The
#' `cards` object is keyed by card code, so it must be walked by name --
#' `jsonlite`'s data-frame simplification would otherwise produce one
#' COLUMN per card code.
#' @param path Character. Path to mwl.json.
#' @return A list with `mwl` (no `format` column -- see below) and
#'   `mwl_card` tibbles.
#' @keywords internal
read_mwl <- function(path) {
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  # No `format` column here: mwl.json has no such field and it is no
  # longer guessed from the code prefix. build_cardpool() attaches it by
  # joining to the v2 restriction table via mwl_v2_format().
  mwl <- tibble::tibble(
    code = vapply(parsed, function(m) as.character(m$code), character(1)),
    name = vapply(parsed, function(m) as.character(m$name), character(1)),
    date_start = vapply(parsed, function(m) as.character(m$date_start), character(1))
  )

  int_or_na <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)

  mwl_card <- purrr::map_dfr(parsed, function(m) {
    cards <- m$cards %||% list()
    if (!length(cards)) {
      return(tibble::tibble(
        mwl_code = character(0), card_code = character(0), deck_limit = integer(0),
        is_restricted = integer(0), universal_faction_cost = integer(0), global_penalty = integer(0)
      ))
    }
    # unname(): `cards` is a JSON object, so vapply() over it returns a
    # vector NAMED by card code, which would otherwise ride into the
    # column as stray names.
    pluck_int <- function(key) unname(vapply(cards, function(v) int_or_na(v[[key]]), integer(1)))

    tibble::tibble(
      mwl_code = as.character(m$code),
      card_code = names(cards),
      deck_limit = pluck_int("deck_limit"),
      is_restricted = pluck_int("is_restricted"),
      universal_faction_cost = pluck_int("universal_faction_cost"),
      global_penalty = pluck_int("global_penalty")
    )
  })

  list(
    mwl = dplyr::arrange(mwl, .data$code),
    mwl_card = dplyr::arrange(mwl_card, .data$mwl_code, .data$card_code)
  )
}

#' Select a table's allowlisted columns and report any dropped upstream keys
#'
#' Uses any_of() rather than ABR's all_of(), and backfills any allowlisted
#' column absent from every row of this batch with NA: unlike the
#' ABR_TOURNAMENT_ALLOWLIST case (fixed personal-data fields, always
#' present), a cardpool JSON record's own optional fields (a card with no
#' `cost`/`strength`, e.g. an identity) can be entirely absent from
#' jsonlite::fromJSON()'s flattened output rather than merely NA-valued,
#' and the schema (inst/sql/schema/cardpool.sql) declares these nullable,
#' not required.
#' @param df A tibble parsed from upstream JSON.
#' @param allowlist Character vector of columns to keep.
#' @param table_name Character. Used only in the unknown-key message.
#' @return A list with `data` (the selected tibble, with every allowlisted
#'   column present) and `unknown_keys` (character vector of upstream
#'   column names not in `allowlist`).
#' @keywords internal
select_cardpool_columns <- function(df, allowlist, table_name) {
  unknown_keys <- setdiff(names(df), allowlist)
  if (length(unknown_keys) > 0) {
    cli::cli_alert_warning(
      "cardpool {table_name}: unrecognized upstream keys dropped: {paste(unknown_keys, collapse = ', ')}"
    )
  }
  selected <- dplyr::select(df, dplyr::any_of(allowlist))
  for (col in setdiff(allowlist, names(selected))) {
    selected[[col]] <- NA
  }
  list(
    data = dplyr::select(selected, dplyr::all_of(allowlist)),
    unknown_keys = if (length(unknown_keys) == 0) character(0) else paste0(table_name, ".", unknown_keys)
  )
}

#' Flatten the checked-out cardpool tree into the relational card schema
#'
#' Flattens the mixed layout of flat cycles/factions/packs JSON plus a
#' pack directory of per-pack card files via purrr::map_dfr() over
#' fs::dir_ls(recurse = TRUE), with explicit column types and
#' dplyr::arrange() row ordering before each dbWriteTable() call inside
#' one transaction, so byte-identical raw content always produces a
#' byte-identical processed database regardless of directory listing order.
#' Each table is passed through its allowlist (above) before being
#' written, since the upstream JSON carries materially more fields than
#' inst/sql/schema/cardpool.sql declares.
#' @keywords internal
build_cardpool <- function(lineage, staged_raw) {
  raw_dir <- staged_raw$raw_dir

  cycles <- read_json_tibble(file.path(raw_dir, "cycles.json"))
  factions <- read_json_tibble(file.path(raw_dir, "factions.json"))
  # Upstream names this field side_code, matching the card table's own
  # side_code column; the cardpool schema's faction table names it side
  # instead (inst/sql/schema/cardpool.sql), predating this fix and left
  # unchanged here rather than migrated, since nothing else depends on
  # the upstream name. any_of() so a fixture/test tibble without
  # side_code at all (already exercised by CARDPOOL_FACTION_ALLOWLIST's
  # NA-backfill path below) isn't broken by the rename.
  if ("side_code" %in% names(factions)) {
    factions <- dplyr::rename(factions, side = "side_code")
  }
  packs <- read_json_tibble(file.path(raw_dir, "packs.json"))

  pack_card_files <- fs::dir_ls(file.path(raw_dir, "pack"), recurse = TRUE, glob = "*.json")
  cards <- purrr::map_dfr(pack_card_files, read_json_tibble)

  cycles_sel <- select_cardpool_columns(cycles, CARDPOOL_CYCLE_ALLOWLIST, "cycle")
  factions_sel <- select_cardpool_columns(factions, CARDPOOL_FACTION_ALLOWLIST, "faction")
  packs_sel <- select_cardpool_columns(packs, CARDPOOL_PACK_ALLOWLIST, "pack")
  cards_sel <- select_cardpool_columns(cards, CARDPOOL_CARD_ALLOWLIST, "card")

  cards <- dplyr::arrange(cards_sel$data, .data$code)
  packs <- dplyr::arrange(packs_sel$data, .data$code)
  cycles <- dplyr::arrange(cycles_sel$data, .data$code)
  factions <- dplyr::arrange(factions_sel$data, .data$code)

  unknown_keys <- c(
    cycles_sel$unknown_keys, factions_sel$unknown_keys,
    packs_sel$unknown_keys, cards_sel$unknown_keys
  )
  unknown_key_check <- list(
    check = "unknown_upstream_keys",
    status = if (length(unknown_keys) == 0) "pass" else "warn",
    message = if (length(unknown_keys) == 0) "ok" else sprintf("Unrecognized upstream keys dropped: %s", paste(unknown_keys, collapse = ", "))
  )

  # Legality data. Both files sit at the repo root alongside cycles.json;
  # absent files are tolerated (an older mirrored commit predates them)
  # rather than aborting the whole cardpool build over a secondary table.
  rotations_path <- file.path(raw_dir, "rotations.json")
  mwl_path <- file.path(raw_dir, "mwl.json")
  rotations <- if (fs::file_exists(rotations_path)) {
    read_rotations(rotations_path)
  } else {
    list(rotation = tibble::tibble(code = character(0), name = character(0), date_start = character(0)),
         rotation_cycle = tibble::tibble(rotation_code = character(0), cycle_code = character(0)))
  }
  mwls <- if (fs::file_exists(mwl_path)) {
    read_mwl(mwl_path)
  } else {
    list(mwl = tibble::tibble(code = character(0), name = character(0), date_start = character(0)),
         mwl_card = tibble::tibble(mwl_code = character(0), card_code = character(0), deck_limit = integer(0),
                                    is_restricted = integer(0), universal_faction_cost = integer(0), global_penalty = integer(0)))
  }

  # The authoritative v2 tree. Tolerated as absent on the same reasoning
  # as the legacy files above -- an older mirrored commit predates it --
  # in which case every table below is empty and mwl$format is NA, which
  # the join check then reports.
  v2_dir <- file.path(raw_dir, "v2")
  formats <- read_formats(file.path(v2_dir, "formats"))
  pools <- read_card_pools(file.path(v2_dir, "card_pools"))
  restrictions <- read_restrictions(file.path(v2_dir, "restrictions"))
  card_sets_path <- file.path(v2_dir, "card_sets.json")
  card_sets <- if (fs::file_exists(card_sets_path)) {
    read_card_sets(card_sets_path)
  } else {
    tibble::tibble(id = character(0), name = character(0), legacy_code = character(0),
                   card_cycle_id = character(0), date_release = character(0),
                   position = integer(0))
  }
  printings <- read_printings(file.path(v2_dir, "printings"))

  # The legacy mwl table's format column, resolved rather than guessed.
  mwls$mwl$format <- mwl_v2_format(mwls$mwl$code, restrictions$restriction)
  mwls$mwl <- dplyr::relocate(mwls$mwl, "format", .after = "name")

  unmatched <- sort(mwls$mwl$code[is.na(mwls$mwl$format)])
  legality_check <- list(
    check = "mwl_v2_format_join",
    status = if (length(unmatched) == 0) "pass" else "warn",
    message = if (length(unmatched) == 0) {
      sprintf("%d rotations, %d ban lists, %d formats, %d restrictions",
              nrow(rotations$rotation), nrow(mwls$mwl),
              nrow(formats$format), nrow(restrictions$restriction))
    } else {
      sprintf("mwl.json codes with no matching v2 restriction: %s",
              paste(unmatched, collapse = ", "))
    }
  )

  # Upstream's own `active` flag against the date-derived answer. They
  # disagree whenever a snapshot has started but the flag has not been
  # moved, which is a maintenance lag in the source, not a bug here --
  # hence a note-shaped warn rather than an abort, and hence
  # active_snapshot() going by date.
  flagged <- formats$format_snapshot$id[formats$format_snapshot$is_active == 1L]
  by_date <- unlist(lapply(
    split(formats$format_snapshot, formats$format_snapshot$format_id),
    function(snaps) {
      started <- snaps[as.Date(snaps$date_start) <= Sys.Date(), , drop = FALSE]
      if (!nrow(started)) return(NULL)
      started$id[which.max(as.Date(started$date_start))]
    }
  ), use.names = FALSE)
  stale_flags <- sort(setdiff(by_date, flagged))
  active_flag_check <- list(
    check = "snapshot_active_flag",
    status = if (length(stale_flags) == 0) "pass" else "warn",
    message = if (length(stale_flags) == 0) {
      sprintf("%d snapshots, upstream active flag agrees with date", nrow(formats$format_snapshot))
    } else {
      sprintf("Snapshots in force by date but not flagged active upstream: %s",
              paste(stale_flags, collapse = ", "))
    }
  )

  db_path <- file.path(dirname(raw_dir), "processed", "cardpool.sqlite")
  fs::dir_create(dirname(db_path))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  apply_schema(con, "cardpool")

  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "cycle", cycles, append = TRUE)
    DBI::dbWriteTable(con, "faction", factions, append = TRUE)
    DBI::dbWriteTable(con, "pack", packs, append = TRUE)
    DBI::dbWriteTable(con, "card", cards, append = TRUE)
    DBI::dbWriteTable(con, "rotation", rotations$rotation, append = TRUE)
    DBI::dbWriteTable(con, "rotation_cycle", rotations$rotation_cycle, append = TRUE)
    DBI::dbWriteTable(con, "mwl", mwls$mwl, append = TRUE)
    DBI::dbWriteTable(con, "mwl_card", mwls$mwl_card, append = TRUE)
    DBI::dbWriteTable(con, "format", formats$format, append = TRUE)
    DBI::dbWriteTable(con, "card_set", card_sets, append = TRUE)
    DBI::dbWriteTable(con, "printing", printings, append = TRUE)
    DBI::dbWriteTable(con, "card_pool", pools$card_pool, append = TRUE)
    DBI::dbWriteTable(con, "card_pool_set", pools$card_pool_set, append = TRUE)
    DBI::dbWriteTable(con, "card_pool_cycle", pools$card_pool_cycle, append = TRUE)
    DBI::dbWriteTable(con, "restriction", restrictions$restriction, append = TRUE)
    DBI::dbWriteTable(con, "restriction_card", restrictions$restriction_card, append = TRUE)
    DBI::dbWriteTable(con, "restriction_subtype", restrictions$restriction_subtype, append = TRUE)
    # After restriction, which format_snapshot.restriction_id references.
    DBI::dbWriteTable(con, "format_snapshot", formats$format_snapshot, append = TRUE)
  })

  br <- build_revision(lineage, build_module_path = "R/build-cardpool.R")

  list(
    db_path = db_path,
    build_revision = br,
    # Composite <source_revision>-b<build_revision prefix>, not the plain
    # UTC-timestamp release_id used by other lineages: cardpool/implementation
    # release identity must track the exact git commit fetched, since the
    # underlying repo can advance between two builds of the same content.
    release_id = sprintf("%s-b%s", staged_raw$source_revision, substr(br, 1, 12)),
    checks = list(unknown_key_check, legality_check, active_flag_check)
  )
}

#' Parse one JSON file into a tibble
#' @param path Character. Path to a JSON file.
#' @return A tibble of the parsed JSON content.
#' @keywords internal
read_json_tibble <- function(path) {
  parsed <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE, flatten = TRUE)
  tibble::as_tibble(parsed)
}

#' Apply a lineage's DDL to a fresh SQLite connection
#' @name apply_schema
#' @param con A live DBI connection.
#' @param lineage_name Character. Lineage name whose schema file to apply.
#' @return TRUE, invisibly, if a schema file was found and applied; FALSE
#'   invisibly otherwise.
#' @keywords internal
apply_schema <- function(con, lineage_name) {
  schema_path <- fs::path_package("netrunneR", "sql", "schema", paste0(lineage_name, ".sql"))
  if (!fs::file_exists(schema_path)) return(invisible(FALSE))
  ddl <- readLines(schema_path, warn = FALSE)
  # Strip full-line "-- ..." comments before splitting on ";" -- a comment
  # containing a semicolon (e.g. explanatory prose) would otherwise split a
  # single statement into malformed fragments.
  ddl <- ddl[!grepl("^\\s*--", ddl)]
  statements <- strsplit(paste(ddl, collapse = "\n"), ";")[[1]]
  for (stmt in statements) {
    stmt <- trimws(stmt)
    if (nzchar(stmt)) DBI::dbExecute(con, stmt)
  }
  invisible(TRUE)
}

# ---- v2 formats / card pools / restrictions ----------------------------
#
# The mirrored repo carries two parallel descriptions of legality. The
# top-level mwl.json / rotations.json read above are the legacy pair; the
# v2/ tree below is the authoritative one, and is what these readers
# flatten. See the header comment in inst/sql/schema/cardpool.sql for why
# the prefix heuristic these replace was wrong.
#
# All of them walk objects by name rather than letting jsonlite simplify:
# a restriction's `points` / `universal_faction_cost` / `global_penalty`
# are JSON OBJECTS keyed by the numeric value, exactly the nested shape
# that mangles into one column per key (the same hazard read_mwl()
# documents for `cards`).

#' Read one JSON file, unsimplified
#' @keywords internal
read_json_raw <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = FALSE)
}

#' Every .json file inside a v2 subdirectory, in sorted order
#'
#' Sorted so a directory listing's order can never leak into the built
#' database, the same guarantee build_cardpool() makes for pack files.
#' @keywords internal
v2_files <- function(dir, recurse = FALSE) {
  if (!fs::dir_exists(dir)) return(character(0))
  sort(as.character(fs::dir_ls(dir, recurse = recurse, glob = "*.json", type = "file")))
}

#' Flatten v2/formats into format / format_snapshot tables
#'
#' Upstream shape: one file per format, `{id, name, snapshots: [{id,
#' date_start, card_pool_id, restriction_id?, active?}]}`. A snapshot
#' without `restriction_id` is a real state, not missing data -- core,
#' system_gateway and ram define a card pool and no ban list.
#' @param dir Character. Path to the v2/formats directory.
#' @return A list with `format` and `format_snapshot` tibbles.
#' @keywords internal
read_formats <- function(dir) {
  files <- v2_files(dir)
  formats <- purrr::map_dfr(files, function(path) {
    f <- read_json_raw(path)
    tibble::tibble(id = as.character(f$id), name = as.character(f$name))
  })
  snapshots <- purrr::map_dfr(files, function(path) {
    f <- read_json_raw(path)
    snaps <- f$snapshots %||% list()
    if (!length(snaps)) return(NULL)
    tibble::tibble(
      id = vapply(snaps, function(s) as.character(s$id), character(1)),
      format_id = as.character(f$id),
      date_start = vapply(snaps, function(s) as.character(s$date_start), character(1)),
      card_pool_id = vapply(snaps, function(s) as.character(s$card_pool_id), character(1)),
      restriction_id = vapply(snaps, function(s) s$restriction_id %||% NA_character_, character(1)),
      is_active = vapply(snaps, function(s) as.integer(isTRUE(s$active)), integer(1))
    )
  })
  if (!nrow(formats)) formats <- tibble::tibble(id = character(0), name = character(0))
  if (!nrow(snapshots)) {
    snapshots <- tibble::tibble(
      id = character(0), format_id = character(0), date_start = character(0),
      card_pool_id = character(0), restriction_id = character(0), is_active = integer(0)
    )
  }
  list(
    format = dplyr::arrange(formats, .data$id),
    format_snapshot = dplyr::arrange(snapshots, .data$format_id, .data$date_start, .data$id)
  )
}

#' Flatten v2/card_pools into card_pool / card_pool_set / card_pool_cycle
#'
#' Upstream shape: one file per format holding an ARRAY of pools,
#' `{id, name, format_id, card_set_ids: [...], card_cycle_ids: [...]}`.
#' @param dir Character. Path to the v2/card_pools directory.
#' @return A list with `card_pool`, `card_pool_set` and `card_pool_cycle`
#'   tibbles.
#' @keywords internal
read_card_pools <- function(dir) {
  pools <- unlist(lapply(v2_files(dir), read_json_raw), recursive = FALSE)

  card_pool <- if (!length(pools)) {
    tibble::tibble(id = character(0), format_id = character(0), name = character(0))
  } else {
    tibble::tibble(
      id = vapply(pools, function(p) as.character(p$id), character(1)),
      format_id = vapply(pools, function(p) as.character(p$format_id), character(1)),
      name = vapply(pools, function(p) as.character(p$name), character(1))
    )
  }

  long <- function(field, out_name) {
    rows <- purrr::map_dfr(pools, function(p) {
      values <- unlist(p[[field]] %||% list(), use.names = FALSE)
      if (!length(values)) return(NULL)
      tibble::tibble(card_pool_id = as.character(p$id), value = as.character(values))
    })
    if (!nrow(rows)) rows <- tibble::tibble(card_pool_id = character(0), value = character(0))
    names(rows)[names(rows) == "value"] <- out_name
    dplyr::arrange(rows, .data$card_pool_id, .data[[out_name]])
  }

  list(
    card_pool = dplyr::arrange(card_pool, .data$id),
    card_pool_set = long("card_set_ids", "card_set_id"),
    card_pool_cycle = long("card_cycle_ids", "card_cycle_id")
  )
}

#' Flatten v2/restrictions into restriction / restriction_card / restriction_subtype
#'
#' Upstream shape: `v2/restrictions/<format>/<id>.json`, each
#' `{id, name, format_id, date_start, banned?: [card_id], restricted?:
#' [card_id], universal_faction_cost?/global_penalty?/points?:
#' {value: [card_id]}, subtypes?: {kind: [subtype_id]}, point_limit?,
#' max_3_point_agendas?}`.
#'
#' The directory a file sits in is NOT the format: several files under
#' `standard/` declare `format_id: "standard"` while carrying a
#' `startup_`-prefixed name. The declared field wins; the directory is
#' ignored entirely.
#' @param dir Character. Path to the v2/restrictions directory.
#' @return A list with `restriction`, `restriction_card` and
#'   `restriction_subtype` tibbles.
#' @keywords internal
read_restrictions <- function(dir) {
  files <- v2_files(dir, recurse = TRUE)
  parsed <- lapply(files, read_json_raw)

  int_or_na <- function(x) if (is.null(x)) NA_integer_ else as.integer(x)

  restriction <- if (!length(parsed)) {
    tibble::tibble(id = character(0), name = character(0), format_id = character(0),
                   date_start = character(0), point_limit = integer(0),
                   max_3_point_agendas = integer(0))
  } else {
    tibble::tibble(
      id = vapply(parsed, function(r) as.character(r$id), character(1)),
      name = vapply(parsed, function(r) as.character(r$name), character(1)),
      format_id = vapply(parsed, function(r) as.character(r$format_id), character(1)),
      date_start = vapply(parsed, function(r) as.character(r$date_start), character(1)),
      point_limit = vapply(parsed, function(r) int_or_na(r$point_limit), integer(1)),
      max_3_point_agendas = vapply(parsed, function(r) int_or_na(r$max_3_point_agendas), integer(1))
    )
  }

  # Array fields: a flat list of card ids, one implied value.
  flag_rows <- function(r, field, column) {
    ids <- unlist(r[[field]] %||% list(), use.names = FALSE)
    if (!length(ids)) return(NULL)
    out <- tibble::tibble(restriction_id = as.character(r$id), card_id = as.character(ids))
    out[[column]] <- 1L
    out
  }

  # Object fields: keyed BY the numeric value, each holding the card ids
  # carrying it. names() is the value, not a card id -- inverting this
  # is the whole reason these cannot be read with simplifyVector.
  value_rows <- function(r, field, column) {
    groups <- r[[field]] %||% list()
    if (!length(groups)) return(NULL)
    purrr::imap_dfr(groups, function(ids, value) {
      ids <- unlist(ids, use.names = FALSE)
      if (!length(ids)) return(NULL)
      out <- tibble::tibble(restriction_id = as.character(r$id), card_id = as.character(ids))
      out[[column]] <- as.integer(value)
      out
    })
  }

  card_rows <- purrr::map(parsed, function(r) {
    parts <- list(
      flag_rows(r, "banned", "is_banned"),
      flag_rows(r, "restricted", "is_restricted"),
      value_rows(r, "universal_faction_cost", "universal_faction_cost"),
      value_rows(r, "global_penalty", "global_penalty"),
      value_rows(r, "points", "points")
    )
    parts <- parts[!vapply(parts, is.null, logical(1))]
    if (!length(parts)) return(NULL)
    # One card can appear under several fields of the SAME list, so the
    # parts are joined into one wide row per card rather than stacked --
    # stacking would break the (restriction_id, card_id) primary key.
    purrr::reduce(parts, dplyr::full_join, by = c("restriction_id", "card_id"))
  })
  card_rows <- card_rows[!vapply(card_rows, is.null, logical(1))]

  restriction_card <- if (!length(card_rows)) {
    tibble::tibble(restriction_id = character(0), card_id = character(0))
  } else {
    dplyr::bind_rows(card_rows)
  }
  for (column in c("is_banned", "is_restricted", "universal_faction_cost",
                   "global_penalty", "points")) {
    if (is.null(restriction_card[[column]])) restriction_card[[column]] <- NA_integer_
    restriction_card[[column]] <- as.integer(restriction_card[[column]])
  }
  restriction_card <- dplyr::select(
    restriction_card, "restriction_id", "card_id", "is_banned", "is_restricted",
    "universal_faction_cost", "global_penalty", "points"
  )

  restriction_subtype <- purrr::map_dfr(parsed, function(r) {
    kinds <- r$subtypes %||% list()
    if (!length(kinds)) return(NULL)
    purrr::imap_dfr(kinds, function(ids, kind) {
      ids <- unlist(ids, use.names = FALSE)
      if (!length(ids)) return(NULL)
      tibble::tibble(restriction_id = as.character(r$id),
                     subtype_id = as.character(ids), kind = as.character(kind))
    })
  })
  if (!nrow(restriction_subtype)) {
    restriction_subtype <- tibble::tibble(restriction_id = character(0),
                                          subtype_id = character(0), kind = character(0))
  }

  list(
    restriction = dplyr::arrange(restriction, .data$id),
    restriction_card = dplyr::arrange(restriction_card, .data$restriction_id, .data$card_id),
    restriction_subtype = dplyr::arrange(restriction_subtype, .data$restriction_id,
                                         .data$kind, .data$subtype_id)
  )
}

#' Flatten v2/card_sets.json into the card_set table
#'
#' `legacy_code` is the v1 pack code, which is what makes a card pool's
#' card_set_ids resolvable against the card table.
#' @param path Character. Path to v2/card_sets.json.
#' @return A card_set tibble.
#' @keywords internal
read_card_sets <- function(path) {
  parsed <- read_json_raw(path)
  if (!length(parsed)) {
    return(tibble::tibble(id = character(0), name = character(0), legacy_code = character(0),
                          card_cycle_id = character(0), date_release = character(0),
                          position = integer(0)))
  }
  chr <- function(field) vapply(parsed, function(s) s[[field]] %||% NA_character_, character(1))
  out <- tibble::tibble(
    id = chr("id"),
    name = chr("name"),
    legacy_code = chr("legacy_code"),
    card_cycle_id = chr("card_cycle_id"),
    date_release = chr("date_release"),
    position = vapply(parsed, function(s) if (is.null(s$position)) NA_integer_ else as.integer(s$position), integer(1))
  )
  dplyr::arrange(out, .data$id)
}

#' Flatten v2/printings into the printing table
#'
#' Each file is an array of printings for one card set; `id` is the
#' printing code the card table is keyed by, `card_id` the v2 slug that
#' restrictions speak.
#' @param dir Character. Path to the v2/printings directory.
#' @return A printing tibble.
#' @keywords internal
read_printings <- function(dir) {
  rows <- unlist(lapply(v2_files(dir, recurse = TRUE), read_json_raw), recursive = FALSE)
  if (!length(rows)) {
    return(tibble::tibble(code = character(0), card_id = character(0), card_set_id = character(0)))
  }
  out <- tibble::tibble(
    code = vapply(rows, function(p) as.character(p$id), character(1)),
    card_id = vapply(rows, function(p) as.character(p$card_id), character(1)),
    card_set_id = vapply(rows, function(p) as.character(p$card_set_id), character(1))
  )
  dplyr::arrange(out, .data$code)
}

#' Resolve each legacy mwl.json code to its v2 restriction's format
#'
#' The two id conventions differ only in separators -- `NAPD_MWL_1.0`
#' against `napd_mwl_1_0`, `standard-ban-list-26-03` against
#' `standard_ban_list_26_03` -- so normalizing `-` and `.` to `_` and
#' lowercasing matches all 41 legacy entries against the v2 set.
#'
#' Returns NA for an entry with no v2 counterpart rather than falling
#' back to a guess: an unmatched code means the two trees have diverged,
#' which the build's mwl_v2_format_join check must surface.
#' @param code Character vector of legacy mwl codes.
#' @param restriction The v2 `restriction` tibble.
#' @return Character vector of format ids, NA where unmatched.
#' @keywords internal
mwl_v2_format <- function(code, restriction) {
  normalize <- function(x) tolower(gsub("[-.]", "_", x))
  if (!nrow(restriction)) return(rep(NA_character_, length(code)))
  restriction$format_id[match(normalize(code), normalize(restriction$id))]
}
