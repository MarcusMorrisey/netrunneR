#' Run the interruptible ABR backfill
#'
#' Writes only into the content-addressed object pool, checkpointing
#' progress in a small tibble persisted beside the pool so a restart
#' refetches nothing already resolved, and promotes a release only once
#' every tournament has a resolved object. This keeps a mistake in a
#' roughly 4400-request, two-second-paced crawl against a volunteer-run
#' server cheap to resume rather than expensive to redo from scratch.
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
    entries <- abr_get(lineage$base_url, sprintf("/tournaments/%s/entries", id))
    object_path <- file.path(pool_dir, paste0(id, ".json"))
    jsonlite::write_json(entries, object_path, auto_unbox = TRUE)
    Sys.chmod(object_path, mode = "0600")

    checkpoint <- dplyr::bind_rows(checkpoint, tibble::tibble(tournament_id = id, resolved = TRUE))
    saveRDS(checkpoint, checkpoint_path)
  }

  all_resolved <- all(tournament_ids %in% checkpoint$tournament_id[checkpoint$resolved])
  invisible(all_resolved)
}
