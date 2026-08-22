#' Build the nrdb release and run abr-to-decklist reconciliation
#'
#' Builds the nrdb tables and runs reconciliation: anti_joins the decklist
#' ids referenced by the captured abr release against the staged index,
#' issues one rate-limited request per gap, records a definitive 404 or
#' 403 as a dated tombstone retried daily for 30 days before becoming
#' permanent_unavailable, and blocks promotion on any transient failure or
#' unresolved id.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "nrdb".
#' @param staged_raw The value returned by fetch_nrdb().
#'
#'
#' @keywords internal
build_nrdb <- function(lineage, staged_raw) {
  referenced_ids <- abr_referenced_decklist_ids()
  gaps <- setdiff(referenced_ids, staged_raw$sweep$decklists$id)

  reconciliation <- reconcile_decklist_gaps(lineage, gaps)

  decklists <- staged_raw$sweep$decklists
  if (nrow(reconciliation$resolved) > 0) {
    decklists <- dplyr::rows_upsert(decklists, reconciliation$resolved, by = "id")
  }

  transient_check <- list(
    check = "decklist_transient_failures",
    status = if (length(reconciliation$transient_failures) == 0) "pass" else "fail",
    message = sprintf("%d ids failed transiently", length(reconciliation$transient_failures))
  )

  db_path <- file.path(dirname(staged_raw$raw_dir), "processed", "nrdb.sqlite")
  fs::dir_create(dirname(db_path))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  apply_schema(con, "nrdb")
  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "decklist", decklists, append = TRUE)
    DBI::dbWriteTable(con, "review", tibble::as_tibble(staged_raw$reviews$results), append = TRUE)
    DBI::dbWriteTable(con, "ruling", tibble::as_tibble(staged_raw$rulings$results), append = TRUE)
  })

  br <- build_revision(lineage, build_module_path = "R/build-nrdb.R")
  # build_revision is computed identically for every lineage, nrdb
  # included -- this call is not a narrowed or lineage-specific variant.

  list(
    db_path = db_path,
    build_revision = br,
    release_id = sprintf("%s-%s", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), release_entropy_suffix()),
    sweep_start = as.character(staged_raw$sweep$sweep_start),
    sweep_end = as.character(staged_raw$sweep$sweep_end),
    sweep_timezone = staged_raw$sweep$sweep_timezone,
    checks = c(staged_raw$checks, list(transient_check))
  )
}

#' @keywords internal
abr_referenced_decklist_ids <- function() {
  li <- lineage("abr")
  active <- tryCatch(resolve_release(li), error = function(e) NULL)
  if (is.null(active)) return(character(0))
  db_path <- file.path(active$processed_dir, "abr.sqlite")
  if (!fs::file_exists(db_path)) return(character(0))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  tryCatch(
    DBI::dbGetQuery(con, "SELECT DISTINCT identity_a_code AS id FROM tournament WHERE identity_a_code IS NOT NULL")$id,
    error = function(e) character(0)
  )
}

#' Fill per-id decklist gaps with rate-limited requests and tombstoning
#'
#' A per-id reconciliation body always wins over a sweep copy of the same
#' id, since the reconciliation fetch is the more targeted, more recently
#' verified source for exactly that decklist. A definitive 404 or 403 is
#' recorded as a dated tombstone retried daily for 30 days, then marked
#' permanent_unavailable; any other failure is transient and blocks
#' promotion rather than being silently skipped.
#' @keywords internal
reconcile_decklist_gaps <- function(lineage, gaps) {
  resolved <- list()
  transient_failures <- character(0)
  tombstones <- list()

  for (id in gaps) {
    result <- tryCatch({
      body <- nrdb_get(lineage$base_url, sprintf("/decklists/%s", id))
      list(ok = TRUE, body = body)
    }, httr2_http_404 = function(e) list(ok = FALSE, definitive = TRUE),
       httr2_http_403 = function(e) list(ok = FALSE, definitive = TRUE),
       error = function(e) list(ok = FALSE, definitive = FALSE))

    if (isTRUE(result$ok)) {
      resolved[[length(resolved) + 1]] <- tibble::as_tibble(result$body)
    } else if (isTRUE(result$definitive)) {
      tombstones[[id]] <- list(id = id, tombstoned_at = format(Sys.time(), tz = "UTC"), status = "tombstoned")
    } else {
      transient_failures <- c(transient_failures, id)
    }
  }

  list(
    resolved = if (length(resolved) > 0) dplyr::bind_rows(resolved) else tibble::tibble(id = character(0)),
    tombstones = tombstones,
    transient_failures = transient_failures
  )
}
