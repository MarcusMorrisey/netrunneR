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

#' Flatten the checked-out cardpool tree into the relational card schema
#'
#' Flattens the mixed layout of flat cycles/factions/packs JSON plus a
#' pack directory of per-pack card files via purrr::map_dfr() over
#' fs::dir_ls(recurse = TRUE), with explicit column types and
#' dplyr::arrange() row ordering before each dbWriteTable() call inside
#' one transaction, so byte-identical raw content always produces a
#' byte-identical processed database regardless of directory listing order.
#' @keywords internal
build_cardpool <- function(lineage, staged_raw) {
  raw_dir <- staged_raw$raw_dir

  cycles <- read_json_tibble(file.path(raw_dir, "cycles.json"))
  factions <- read_json_tibble(file.path(raw_dir, "factions.json"))
  packs <- read_json_tibble(file.path(raw_dir, "packs.json"))

  pack_card_files <- fs::dir_ls(file.path(raw_dir, "pack"), recurse = TRUE, glob = "*.json")
  cards <- purrr::map_dfr(pack_card_files, read_json_tibble)

  cards <- dplyr::arrange(cards, .data$code)
  packs <- dplyr::arrange(packs, .data$code)
  cycles <- dplyr::arrange(cycles, .data$code)
  factions <- dplyr::arrange(factions, .data$code)

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
    checks = list()
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
  statements <- strsplit(paste(ddl, collapse = "\n"), ";")[[1]]
  for (stmt in statements) {
    stmt <- trimws(stmt)
    if (nzchar(stmt)) DBI::dbExecute(con, stmt)
  }
  invisible(TRUE)
}
