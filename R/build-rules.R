#' Build the rules release tables and validate checks
#'
#' Builds the rules release tables from the parsed hub index and the
#' pooled PDF hashes, and supplies the lineage's validate checks warning
#' when the hub's own listing order disagrees with publish-date order
#' (the hub order is what's written and promoted either way -- see
#' check_version_monotonic()), and requiring every recorded pdf_url's
#' current bytes to match its stored hash, blocking promotion rather than
#' overwriting a pool entry.
#'
#' @param lineage A lineage object of class netrunneR_web_archive.
#' @param staged_raw The value returned by fetch_lineage.netrunneR_web_archive().
#' @param ... Ignored.
#'
#' @export
build_lineage.netrunneR_web_archive <- function(lineage, staged_raw, ...) {
  index <- staged_raw$index

  db_path <- fs::path_norm(file.path(staged_raw$raw_dir, "..", "processed", "rules.sqlite"))
  fs::dir_create(dirname(db_path))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  apply_schema(con, "rules")
  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "rules_version", index, append = TRUE)
  })

  monotonic_check <- check_version_monotonic(index)
  hash_check <- check_pdf_hashes(index, staged_raw$raw_dir)

  br <- build_revision(lineage, build_module_path = "R/build-rules.R")

  list(
    db_path = db_path,
    build_revision = br,
    # Plain <UTC timestamp>-<hex suffix> form, unlike cardpool/implementation's
    # <source_revision>-b<build_revision> composite: a web-archive fetch has
    # no single upstream commit to key release identity to, so wall-clock
    # time plus an entropy suffix stands in for it.
    release_id = sprintf("%s-%s", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), release_entropy_suffix()),
    checks = list(monotonic_check, hash_check)
  )
}

#' Cross-check the hub's listing order against publish-date order
#'
#' The hub lists releases newest-first, so a well-formed index's order
#' matches published_date sorted *descending*, not ascending -- comparing
#' against an ascending sort is a bug that fails on every correctly
#' ordered real index with 2+ distinct dates, confirmed live this
#' session with a clean two-row fixture that has zero anomalies.
#'
#' The hub page also carries no actual publish-date field; published_date
#' is sourced from each PDF's HTTP Last-Modified header (see
#' head_last_modified(), R/fetch-web-archive.R), which is only a proxy
#' and can drift from true release order even once the sort direction
#' above is fixed -- confirmed live this session: nullsignal.games has
#' re-uploaded/touched older PDFs (e.g. v1.0's Last-Modified postdates
#' v1.1/1.2/1.3's, and v22.12/v23.08 share an identical Last-Modified
#' date despite being listed newest-first) in ways that don't reflect a
#' real reordering of releases. The hub's own listing order -- what
#' `index` is already in, and what gets written and promoted regardless
#' of this check's outcome -- is trusted as the authoritative version
#' sequence; a disagreement is recorded as a warning for a human to look
#' at, not a promotion-blocking failure, since blocking here would mean
#' this lineage can never promote again once one old PDF's mtime drifts.
#' @param index A tibble with a `version` and `published_date` column, in
#'   the hub's own newest-first listing order.
#' @return A check-result list with `check`, `status`, and `message`.
#' @keywords internal
check_version_monotonic <- function(index) {
  by_date <- dplyr::arrange(index, dplyr::desc(.data$published_date))
  in_order <- identical(index$version, by_date$version)
  list(
    check = "version_monotonic",
    status = if (in_order) "pass" else "warn",
    message = if (in_order) "ok" else sprintf(
      "Hub order and Last-Modified-date order disagree -- hub order: %s; date order: %s",
      paste(index$version, collapse = ", "), paste(by_date$version, collapse = ", ")
    )
  )
}

#' Require every recorded pdf_url's current bytes to match its stored hash
#'
#' A mismatch blocks promotion and leaves the pool object untouched rather
#' than overwriting it, so a corrupted or tampered download can never
#' silently replace a previously verified object in the content-addressed
#' pool.
#' @param index A tibble with `version` and `pooled_hash` columns.
#' @param raw_dir Character. The staged raw directory holding `objects/`.
#' @return A check-result list with `check`, `status`, and `message`.
#' @keywords internal
check_pdf_hashes <- function(index, raw_dir) {
  mismatches <- character(0)
  for (i in seq_len(nrow(index))) {
    sha <- index$pooled_hash[i]
    object_path <- file.path(raw_dir, "objects", substr(sha, 1, 2), paste0(sha, ".pdf"))
    if (!fs::file_exists(object_path)) {
      mismatches <- c(mismatches, index$version[i])
      next
    }
    actual_sha <- digest::digest(file = object_path, algo = "sha256")
    if (!identical(actual_sha, sha)) mismatches <- c(mismatches, index$version[i])
  }
  list(
    check = "pdf_hash_match",
    status = if (length(mismatches) == 0) "pass" else "fail",
    message = if (length(mismatches) == 0) "ok" else sprintf("Hash mismatch for versions: %s", paste(mismatches, collapse = ", "))
  )
}
