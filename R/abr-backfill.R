#' Run the interruptible ABR backfill
#'
#' Writes only into the content-addressed object pool, checkpointing
#' progress in a small tibble persisted beside the pool so a restart
#' refetches nothing already resolved, and promotes a release only once
#' every tournament has a resolved object. This keeps a mistake in a
#' roughly 4400-request, two-second-paced crawl against a volunteer-run
#' server cheap to resume rather than expensive to redo from scratch.
#'
#' fetch_abr() (R/fetch-abr.R) calls this for the entries step of every
#' sync attempt, scheduled or backfill alike -- checkpointing the
#' expensive crawl is a property of the entries fetch itself, not of
#' mode, since an interrupted "scheduled" run loses exactly the same
#' unresolved work as an interrupted "backfill" one. It stays exported
#' and independently callable so a specific set of tournament ids can
#' also be re-resolved directly, e.g. after a data issue narrower than a
#' full sync attempt.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "abr".
#' @param tournament_ids Character vector of tournament ids to backfill.
#'
#' pool_dir and each object file it writes are 0700/0600 (owner-only):
#' backfilled entries are unprocessed ABR provenance data that has not
#' yet passed build_abr()'s two-layer allowlist and deny-pattern check.
#' (ref: DL-002)
#'
#' @return Invisibly, TRUE once every id in tournament_ids has a resolved
#'   checkpoint entry, FALSE if the backfill was interrupted before that.
#' @export
run_abr_backfill <- function(lineage, tournament_ids) {
  # The real ABR /tournaments/results endpoint returns id as a JSON
  # number, not a string (confirmed live), so jsonlite::fromJSON()
  # parses tournaments$id as an integer column -- coerce to character
  # here to match this function's own documented contract, since the
  # checkpoint tibble below is seeded character(0) and an integer id
  # otherwise fails dplyr::bind_rows() with a type-mismatch error on the
  # very first real (non-fixture) backfill run.
  tournament_ids <- as.character(tournament_ids)

  pool_dir <- file.path(lineage$store_root, "objects")
  fs::dir_create(pool_dir, mode = "0700")

  checkpoint_path <- file.path(lineage$store_root, "backfill-checkpoint.rds")
  checkpoint <- if (fs::file_exists(checkpoint_path)) {
    readRDS(checkpoint_path)
  } else {
    tibble::tibble(tournament_id = character(0), resolved = logical(0))
  }

  remaining <- setdiff(tournament_ids, checkpoint$tournament_id[checkpoint$resolved])

  for (id in remaining) {
    entries <- abr_get(lineage$base_url, "/entries", list(id = id))
    object_path <- file.path(pool_dir, paste0(id, ".json"))
    jsonlite::write_json(entries, object_path, auto_unbox = TRUE)
    Sys.chmod(object_path, mode = "0600")

    checkpoint <- dplyr::bind_rows(checkpoint, tibble::tibble(tournament_id = id, resolved = TRUE))
    saveRDS(checkpoint, checkpoint_path)
  }

  all_resolved <- all(tournament_ids %in% checkpoint$tournament_id[checkpoint$resolved])
  invisible(all_resolved)
}

#' Read one tournament's entries back off the checkpointed object pool
#'
#' Companion to run_abr_backfill(): reads the same object_path that
#' function's write_json(..., auto_unbox = TRUE) wrote, with matching
#' simplifyVector = TRUE parsing, so the value fetch_abr() gets back
#' matches the shape abr_get() would have returned directly.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "abr".
#' @param tournament_id Character scalar. Must already have a resolved
#'   checkpoint entry (i.e. run_abr_backfill() has been called with this
#'   id first).
#'
#' @keywords internal
read_backfill_object <- function(lineage, tournament_id) {
  object_path <- file.path(lineage$store_root, "objects", paste0(tournament_id, ".json"))
  jsonlite::fromJSON(object_path, simplifyVector = TRUE)
}
