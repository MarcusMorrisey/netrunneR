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

#' Code prefixes that identify an MWL entry's play format
#'
#' mwl.json carries no `format` field, so it is derived from each entry's
#' code prefix: the 41 upstream entries use exactly `standard-*`,
#' `startup-*`, `sunset-*` and `NAPD_MWL_*`. A prefix outside this set
#' becomes "unknown" and is surfaced by the build check rather than
#' silently bucketed, since a new upstream naming convention is the kind
#' of change that should be noticed, not absorbed.
#' @keywords internal
MWL_FORMAT_PREFIXES <- c(standard = "standard", startup = "startup", sunset = "sunset", napd = "napd")

#' Derive an MWL entry's format from its code
#' @keywords internal
mwl_format_of <- function(code) {
  prefix <- tolower(sub("[-_].*$", "", code))
  unname(MWL_FORMAT_PREFIXES[prefix] %||% "unknown")
}

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
#' @return A list with `mwl` and `mwl_card` tibbles.
#' @keywords internal
read_mwl <- function(path) {
  parsed <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  mwl <- tibble::tibble(
    code = vapply(parsed, function(m) as.character(m$code), character(1)),
    name = vapply(parsed, function(m) as.character(m$name), character(1)),
    format = vapply(parsed, function(m) mwl_format_of(as.character(m$code)), character(1)),
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
    tibble::tibble(
      mwl_code = as.character(m$code),
      card_code = names(cards),
      deck_limit = vapply(cards, function(v) int_or_na(v$deck_limit), integer(1)),
      is_restricted = vapply(cards, function(v) int_or_na(v$is_restricted), integer(1)),
      universal_faction_cost = vapply(cards, function(v) int_or_na(v$universal_faction_cost), integer(1)),
      global_penalty = vapply(cards, function(v) int_or_na(v$global_penalty), integer(1))
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
    list(mwl = tibble::tibble(code = character(0), name = character(0), format = character(0), date_start = character(0)),
         mwl_card = tibble::tibble(mwl_code = character(0), card_code = character(0), deck_limit = integer(0),
                                    is_restricted = integer(0), universal_faction_cost = integer(0), global_penalty = integer(0)))
  }

  unknown_formats <- sort(unique(mwls$mwl$code[mwls$mwl$format == "unknown"]))
  legality_check <- list(
    check = "mwl_format_derivation",
    status = if (length(unknown_formats) == 0) "pass" else "warn",
    message = if (length(unknown_formats) == 0) {
      sprintf("%d rotations, %d ban lists", nrow(rotations$rotation), nrow(mwls$mwl))
    } else {
      sprintf("MWL entries with an unrecognized code prefix: %s", paste(unknown_formats, collapse = ", "))
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
    checks = list(unknown_key_check, legality_check)
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
