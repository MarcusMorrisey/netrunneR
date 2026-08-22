# ND-JSON release ledger: durable, append-only history of promoted,
# no_change, validation_failed and rolled_back events per lineage.
#' Append one ND-JSON record to the release ledger
#'
#' Writes through a low-level file handle, flushes the connection buffer,
#' then fsyncs the file and its containing directory before returning, so
#' a crash immediately after append_ledger() returns cannot lose the
#' record to page-cache buffering, and a rename or unlink racing this
#' append cannot observe a directory entry that was never durably synced.
#' Base R and fs expose no direct fsync(2) syscall binding, so both syncs
#' are performed via the external `sync` binary invoked on each path; a
#' nonzero exit from either call aborts rather than being discarded.
#'
#' @param store_root Character. The lineage's store root.
#' @param record A list to serialize as one ND-JSON line.
#' @export
append_ledger <- function(store_root, record) {
  ledger_path <- file.path(store_root, "releases.jsonl")
  fs::dir_create(store_root)

  line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null")
  con <- file(ledger_path, open = "ab")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(paste0(line, "\n")), con)
  flush(con)

  sync_or_abort(ledger_path)
  sync_or_abort(store_root)

  invisible(record)
}

#' Sync one path to durable storage, aborting on failure
#'
#' A failed sync must surface as an error, not be silently discarded --
#' the caller needs to know append_ledger()'s durability guarantee did not
#' hold for this write.
#' @keywords internal
sync_or_abort <- function(path) {
  status <- system2("sync", args = shQuote(path), stdout = FALSE, stderr = FALSE)
  if (!identical(status, 0L)) {
    cli::cli_abort(
      "sync failed (exit status {status}) for {path}; ledger durability not guaranteed",
      class = "netrunneR_sync_failed"
    )
  }
  invisible(TRUE)
}

#' Read and parse the release ledger
#'
#' @param store_root Character. The lineage's store root.
#'
#' @return A list of parsed ND-JSON records, oldest first.
#' @export
read_ledger <- function(store_root) {
  ledger_path <- file.path(store_root, "releases.jsonl")
  if (!fs::file_exists(ledger_path)) return(list())

  lines <- readLines(ledger_path, warn = FALSE)
  lines <- lines[nzchar(lines)]
  lapply(lines, jsonlite::fromJSON, simplifyVector = FALSE)
}

#' Check that the ledger's last promoted release agrees with active
#'
#' A disagreement at startup is reported and the process exits 3 rather
#' than being auto-repaired: silently picking a side could paper over a
#' promote() that crashed partway, leaving the operator unaware the store
#' needs manual inspection.
#'
#' @param lineage A lineage object.
#' @export
check_ledger_consistency <- function(lineage) {
  records <- read_ledger(lineage$store_root)
  promoted <- Filter(function(r) identical(r$event, "promoted"), records)

  if (length(promoted) == 0) return(invisible(TRUE))

  last_release_id <- promoted[[length(promoted)]]$release_id
  active_link <- file.path(lineage$store_root, "active")

  if (!fs::file_exists(active_link)) {
    cli::cli_abort("Ledger records a promoted release but no active symlink exists", class = "netrunneR_ledger_mismatch")
  }

  active_target <- basename(fs::path_real(active_link))
  if (!identical(active_target, last_release_id)) {
    cli::cli_abort(
      "Ledger/active disagreement: ledger last promoted {last_release_id}, active points at {active_target}",
      class = "netrunneR_ledger_mismatch"
    )
  }

  invisible(TRUE)
}
