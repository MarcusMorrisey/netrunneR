# The lineage-agnostic sync orchestration: lock acquisition, staging,
# no-op short-circuiting, fetch/build/validate dispatch, promote, and
# retention pruning. Every lineage runs the same eight-step sequence
# defined here; only fetch_lineage() and build_lineage() vary by lineage.
#' Run the full sync pipeline for one lineage
#'
#' Executes eight steps identically for every lineage: acquire the
#' non-blocking lock and check free space, dispatch fetch_lineage() into a
#' fresh staging/<attempt_id> directory created mode 2750, compare the
#' (content_identity, build_revision) pair against the active manifest and
#' short-circuit to a no_change ledger record on a match, dispatch
#' build_lineage(), run validate_release(), assemble and write the
#' manifest, promote, then prune old releases and failed staging
#' directories.
#'
#' @param lineage A lineage object.
#' @param mode Character. One of "scheduled", "backfill", "rollback".
#' @param release_id Character. Required (and only meaningful) when
#'   mode = "rollback".
#'
#' @return Invisibly, the ledger record written for this attempt.
#' @export
run_sync <- function(lineage, mode = "scheduled", release_id = NULL) {
  if (identical(mode, "rollback")) {
    rollback(lineage, release_id)
    record <- list(
      event = "rolled_back", lineage = lineage$name, release_id = release_id,
      at = format_utc_now()
    )
    append_ledger(lineage$store_root, record)
    append_sync_log(lineage$store_root, record)
    return(invisible(record))
  }

  lock <- acquire_lock(lineage$store_root)
  if (is.null(lock)) {
    cli::cli_inform("Lock held for lineage {lineage$name}; skipping this run.")
    # Signals a typed condition rather than calling quit() directly, so
    # exit-status 4 stays confined to inst/scripts/sync.R -- no function
    # under R/ terminates the host process.
    rlang::abort(
      sprintf("Lock held for lineage '%s'; skipping this run.", lineage$name),
      class = "netrunneR_lock_contention"
    )
  }
  on.exit(filelock::unlock(lock), add = TRUE)

  # Appends release_entropy_suffix() to the timestamp, not format_utc_now()
  # alone: whole-second resolution otherwise lets a same-second retry
  # after a failed attempt reuse an identical attempt_id and silently mix
  # old and new build inputs in the same staging directory.
  attempt_id <- sprintf("attempt-%s-%s", format_utc_now(), release_entropy_suffix())
  attempt_dir <- file.path(lineage$store_root, "staging", attempt_id)
  fs::dir_create(attempt_dir, mode = "2750")

  staged_raw <- fetch_lineage(lineage, attempt_dir)

  if (no_op_change(lineage, staged_raw, mode)) {
    record <- list(
      event = "no_change", lineage = lineage$name, at = format_utc_now(),
      content_identity = staged_raw$content_identity
    )
    append_ledger(lineage$store_root, record)
    append_sync_log(lineage$store_root, record)
    fs::dir_delete(attempt_dir)
    return(invisible(record))
  }

  built <- build_lineage(lineage, staged_raw)
  report <- validate_release(lineage, built)

  # Same same-second collision risk as attempt_id above applies to the
  # manual/backfill release_id fallback, so it gets the same suffix.
  release_id <- built$release_id %||% sprintf("%s-%s", format_utc_now(), release_entropy_suffix())
  manifest <- list(
    release_id = release_id,
    lineage = lineage$name,
    content_identity = staged_raw$content_identity,
    build_revision = built$build_revision,
    validate_report = report,
    promoted_at = format_utc_now()
  )
  write_manifest(attempt_dir, manifest)

  if (!identical(report$status, "pass")) {
    record <- list(event = "validation_failed", lineage = lineage$name, at = format_utc_now(), report = report)
    append_ledger(lineage$store_root, record)
    append_sync_log(lineage$store_root, record)
    rlang::abort("Validation checks failed; release not promoted", class = "netrunneR_validation_failed")
  }

  promote(lineage$store_root, attempt_dir, release_id)

  record <- list(event = "promoted", lineage = lineage$name, release_id = release_id, at = format_utc_now())
  append_ledger(lineage$store_root, record)
  append_sync_log(lineage$store_root, record)

  prune_releases(lineage$store_root)

  invisible(record)
}

#' Compare staged content against the active manifest's (content_identity,
#' build_revision) pair
#'
#' Mode "scheduled" disables conditional headers and records no_change on
#' a pair match without consuming a retention slot, so a scheduled run
#' that finds nothing new does not crowd out the 30-release retention
#' window with a copy identical to what is active. Comparing the pair
#' rather than content_identity alone catches a shared-code or DDL change
#' with unchanged upstream content: since build_revision() only hashes
#' static package files (not staged_raw), the candidate build_revision can
#' be computed here before build_lineage() runs, at no cost beyond one
#' digest pass.
#' @keywords internal
no_op_change <- function(lineage, staged_raw, mode) {
  if (!identical(mode, "scheduled")) return(FALSE)

  active <- tryCatch(resolve_release(lineage), error = function(e) NULL)
  if (is.null(active)) return(FALSE)

  manifest_path <- file.path(active$release_dir, "manifest.json")
  if (!fs::file_exists(manifest_path)) return(FALSE)

  prev <- jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  candidate_build_revision <- build_revision(lineage, lineage$build_module_path)

  identical(prev$content_identity, staged_raw$content_identity) &&
    identical(prev$build_revision, candidate_build_revision)
}

#' @keywords internal
write_manifest <- function(attempt_dir, manifest) {
  jsonlite::write_json(manifest, file.path(attempt_dir, "manifest.json"), auto_unbox = TRUE, pretty = TRUE)
}

#' Append one line to a lineage's human-readable sync log
#'
#' A plain-text log distinct from the ND-JSON ledger, written under
#' store_root/logs so the capture-boundary sentinel test's file-content
#' scan (raw/, objects/, manifest.json, releases.jsonl, logs) has a real
#' file to scan on this path instead of a directory that never exists.
#' Records only the same event/lineage/release_id/at fields already
#' written to the ledger, never response bytes or headers.
#' @keywords internal
append_sync_log <- function(store_root, record) {
  log_dir <- file.path(store_root, "logs")
  fs::dir_create(log_dir, mode = "0700")
  line <- sprintf(
    "[%s] event=%s lineage=%s%s",
    record$at, record$event, record$lineage,
    if (!is.null(record$release_id)) sprintf(" release_id=%s", record$release_id) else ""
  )
  cat(line, "\n", file = file.path(log_dir, "sync.log"), append = TRUE, sep = "")
}

#' Prune old releases and failed staging directories
#'
#' Keeps 30 complete releases and 5 failed staging directories, and never
#' prunes the directory active currently points at even if it would
#' otherwise fall outside the retention window.
#' @keywords internal
prune_releases <- function(store_root, keep_releases = 30L, keep_failed_staging = 5L) {
  releases_dir <- file.path(store_root, "releases")
  if (fs::dir_exists(releases_dir)) {
    releases <- sort(fs::dir_ls(releases_dir, type = "directory"))
    active_target <- tryCatch(fs::path_real(file.path(store_root, "active")), error = function(e) NA_character_)
    # Canonicalizes every release path the same way active_target already
    # is, so a symlinked mount ancestor or other path-representation
    # difference cannot make the active release fail to string-match and
    # be pruned: setdiff() compares canonical paths, but the paths
    # actually removed stay the original fs::dir_ls() entries.
    releases_real <- vapply(releases, function(p) {
      tryCatch(fs::path_real(p), error = function(e) NA_character_)
    }, character(1))
    prunable <- releases[is.na(releases_real) | !(releases_real %in% active_target)]
    if (length(prunable) > keep_releases) {
      to_remove <- utils::head(prunable, length(prunable) - keep_releases)
      fs::dir_delete(to_remove)
    }
  }

  staging_dir <- file.path(store_root, "staging")
  if (fs::dir_exists(staging_dir)) {
    attempts <- sort(fs::dir_ls(staging_dir, type = "directory"))
    if (length(attempts) > keep_failed_staging) {
      to_remove <- utils::head(attempts, length(attempts) - keep_failed_staging)
      fs::dir_delete(to_remove)
    }
  }
}

#' @keywords internal
format_utc_now <- function() {
  format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC")
}
