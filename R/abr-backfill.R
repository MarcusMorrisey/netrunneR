#' Consecutive 5xx responses from ABR's /entries endpoint treated as a
#' real upstream outage rather than isolated per-tournament breakage.
#' Confirmed live this session: tournament 5379's /entries endpoint
#' 500s consistently and in isolation (curled directly, twice, with
#' every other endpoint healthy) -- a single bad id is not evidence of
#' an outage, so tombstoning past it and continuing is correct, but a
#' run of failures back-to-back looks exactly like the retry-storm
#' scenario abr_get()'s hard-stop exists to protect the volunteer-run
#' server from, and re-raises instead.
#' @keywords internal
ABR_BACKFILL_MAX_CONSECUTIVE_5XX <- 3L

#' Days a persistently-failing tournament id is retried daily before
#' being marked permanent_unavailable and excluded from the release.
#' @keywords internal
ABR_BACKFILL_TOMBSTONE_DAYS <- 30L

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
#' A single tournament id's /entries request can 5xx in isolation --
#' confirmed live this session against a real, otherwise-healthy server
#' -- so an isolated failure is recorded as a dated tombstone and
#' retried at most once per calendar day, same discipline as the
#' now-removed nrdb decklist reconciliation's tombstoning. After
#' ABR_BACKFILL_TOMBSTONE_DAYS of consecutive daily failures the id is
#' marked permanent_unavailable and excluded from the release rather
#' than blocking it forever. A run of ABR_BACKFILL_MAX_CONSECUTIVE_5XX
#' failures in a row among ids being attempted for the first time is
#' treated as a real outage and re-raised, same fail-closed behavior
#' abr_get() already had -- but a failure on an id already known-bad
#' from a prior run (it already has a checkpointed first_failed_at)
#' never counts toward that streak, since re-attempting a small pool of
#' chronically-broken ids failing again is expected, not evidence of a
#' fresh outage; those ids fall through to the tombstone-retry path
#' individually instead. Confirmed live this session: tournament ids
#' 5379, 4134, 2182, 1769 and 1655 have all been failing since
#' 2026-08-23 and, being scattered non-adjacent ids, tripped the old
#' blind consecutive-count guard purely by being retried together.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "abr".
#' @param tournament_ids Character vector of tournament ids to backfill.
#'
#' pool_dir and each object file it writes are 0700/0600 (owner-only):
#' backfilled entries are unprocessed ABR provenance data that has not
#' yet passed build_abr()'s two-layer allowlist and deny-pattern check.
#' (ref: DL-002)
#'
#' @return Invisibly, a list with `all_settled` (TRUE once every id in
#'   tournament_ids is either resolved or permanent_unavailable -- i.e.
#'   nothing is left pending a future retry) and `permanent_ids` (the
#'   subset of tournament_ids marked permanent_unavailable).
#' @export
run_abr_backfill <- function(lineage, tournament_ids) {
  # The real ABR /tournaments/results endpoint returns id as a JSON
  # number, not a string (confirmed live), so jsonlite::fromJSON()
  # parses tournaments$id as an integer column -- coerce to character
  # here to match this function's own documented contract, since the
  # checkpoint tibble below is seeded character(0) and an integer id
  # otherwise fails dplyr::bind_rows()/rows_upsert() with a type-mismatch
  # error on the very first real (non-fixture) backfill run.
  tournament_ids <- as.character(tournament_ids)

  pool_dir <- file.path(lineage$store_root, "objects")
  fs::dir_create(pool_dir, mode = "0700")

  checkpoint_path <- file.path(lineage$store_root, "backfill-checkpoint.rds")
  checkpoint <- if (fs::file_exists(checkpoint_path)) {
    readRDS(checkpoint_path)
  } else {
    tibble::tibble(
      tournament_id = character(0), resolved = logical(0),
      first_failed_at = character(0), last_failed_at = character(0),
      permanent_unavailable = logical(0)
    )
  }
  # A checkpoint written before tombstoning existed only has
  # tournament_id/resolved -- backfill the new columns rather than
  # discard real, already-fetched progress (312 tournaments' entries
  # were on disk under the old schema when this was added).
  for (col in c("first_failed_at", "last_failed_at")) {
    if (!col %in% names(checkpoint)) checkpoint[[col]] <- NA_character_
  }
  if (!"permanent_unavailable" %in% names(checkpoint)) checkpoint$permanent_unavailable <- FALSE

  today <- format(Sys.Date(), "%Y-%m-%d")
  settled_ids <- checkpoint$tournament_id[checkpoint$resolved | checkpoint$permanent_unavailable]
  already_attempted_today <- checkpoint$tournament_id[!is.na(checkpoint$last_failed_at) & checkpoint$last_failed_at == today]
  remaining <- setdiff(tournament_ids, union(settled_ids, already_attempted_today))

  consecutive_failures <- 0L
  # Snapshot which ids already have a prior failure on record *before*
  # this run's loop starts mutating checkpoint -- these are ids being
  # retried out of the tombstone pool, not fresh attempts, and their
  # failures must not feed the outage-detection streak below (see the
  # function doc for why: 5 known-bad, non-adjacent ids retried
  # together used to trip the guard just by being retried in the same
  # batch).
  known_failing_ids <- checkpoint$tournament_id[!is.na(checkpoint$first_failed_at)]

  for (id in remaining) {
    is_retry_of_known_failure <- id %in% known_failing_ids

    result <- tryCatch(
      list(ok = TRUE, entries = abr_get(lineage, "/entries", list(id = id))),
      netrunneR_abr_5xx = function(e) list(ok = FALSE)
    )

    if (isTRUE(result$ok)) {
      object_path <- file.path(pool_dir, paste0(id, ".json"))
      jsonlite::write_json(result$entries, object_path, auto_unbox = TRUE)
      Sys.chmod(object_path, mode = "0600")

      checkpoint <- dplyr::rows_upsert(checkpoint, tibble::tibble(
        tournament_id = id, resolved = TRUE,
        first_failed_at = NA_character_, last_failed_at = NA_character_,
        permanent_unavailable = FALSE
      ), by = "tournament_id")
      consecutive_failures <- 0L
    } else {
      if (!is_retry_of_known_failure) {
        consecutive_failures <- consecutive_failures + 1L
        if (consecutive_failures >= ABR_BACKFILL_MAX_CONSECUTIVE_5XX) {
          rlang::abort(
            sprintf("ABR /entries returned 5xx for %d consecutive tournament ids; treating as an outage, not isolated per-id breakage", consecutive_failures),
            class = "netrunneR_abr_backfill_outage"
          )
        }
      }

      prior <- checkpoint[checkpoint$tournament_id == id, ]
      first_failed_at <- if (nrow(prior) > 0 && !is.na(prior$first_failed_at[1])) prior$first_failed_at[1] else today
      age_days <- as.integer(as.Date(today) - as.Date(first_failed_at))
      permanent <- age_days >= ABR_BACKFILL_TOMBSTONE_DAYS

      checkpoint <- dplyr::rows_upsert(checkpoint, tibble::tibble(
        tournament_id = id, resolved = FALSE,
        first_failed_at = first_failed_at, last_failed_at = today,
        permanent_unavailable = permanent
      ), by = "tournament_id")
    }

    saveRDS(checkpoint, checkpoint_path)
  }

  settled_ids <- checkpoint$tournament_id[checkpoint$resolved | checkpoint$permanent_unavailable]
  all_settled <- all(tournament_ids %in% settled_ids)
  permanent_ids <- intersect(tournament_ids, checkpoint$tournament_id[checkpoint$permanent_unavailable])

  invisible(list(all_settled = all_settled, permanent_ids = permanent_ids))
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
#'   id first and it did not come back permanent_unavailable).
#'
#' @keywords internal
read_backfill_object <- function(lineage, tournament_id) {
  object_path <- file.path(lineage$store_root, "objects", paste0(tournament_id, ".json"))
  jsonlite::fromJSON(object_path, simplifyVector = TRUE)
}
