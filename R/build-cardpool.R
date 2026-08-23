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
    checks = list(unknown_key_check)
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
