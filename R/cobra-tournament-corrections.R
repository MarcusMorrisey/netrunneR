#' Known-bad cobra tournament date corrections
#'
#' Cobra (NSG's own tournament platform) occasionally stores a garbage
#' `date` value for a tournament -- e.g. tournament_id 4910's `date`
#' field reads "1900-07-13" even though its own `created_at` is
#' 2026-07-07T11:50:47.674Z, for a tournament titled "Netrunner PH
#' Online League July 2026". This is a data-quality bug in cobra's own
#' source data (most likely an organizer entering a date without a year,
#' with the platform defaulting to epoch/1900), not something ingestion
#' or the merge can detect algorithmically -- a wrong-but-plausible-
#' looking date wouldn't reliably distinguish itself from a real one.
#' Confirmed corrections are recorded here by `tournament_id`, mirroring
#' the human-verified override pattern already established in
#' `R/abr-cobra-xref.R`, and applied during `build_cobra()` immediately
#' after cobra's raw bundles are flattened into `tournament` -- before
#' either the merge (`merge_abr_cobra()`) or any other downstream table
#' reads `date`, so both cobra's own `tournament` table and the merged
#' `tournament_merged` feed see the corrected value.
#'
#' @param path Character. Defaults to the packaged template.
#' @return A tibble with one row per known-bad (tournament_id, corrected_date).
#' @keywords internal
read_cobra_tournament_date_corrections <- function(path = system.file(
                                                      "extdata", "cobra_tournament_date_corrections.csv",
                                                      package = "netrunneR"
                                                    )) {
  readr::read_csv(
    path,
    col_types = readr::cols(
      tournament_id = readr::col_character(),
      corrected_date = readr::col_character(),
      reason = readr::col_character(),
      verified_by = readr::col_character(),
      verified_at = readr::col_character()
    )
  )
}

#' Apply known cobra tournament date corrections
#'
#' See `read_cobra_tournament_date_corrections()` for why this exists.
#' Matches on `tournament_id`; a correction for an id no longer present
#' in `tournaments` (e.g. it has aged out of cobra's recent-index window)
#' is silently a no-op.
#'
#' @param tournaments The `tournament` data frame built by
#'   `flatten_cobra_tournament()`/`cobra_bind_allowlisted()`.
#' @param corrections Defaults to `read_cobra_tournament_date_corrections()`.
#' @return `tournaments` with corrected `date` values applied.
#' @keywords internal
apply_cobra_tournament_date_corrections <- function(tournaments,
                                                      corrections = read_cobra_tournament_date_corrections()) {
  match_idx <- match(tournaments$tournament_id, corrections$tournament_id)
  hit <- !is.na(match_idx)
  tournaments$date[hit] <- corrections$corrected_date[match_idx[hit]]
  tournaments
}
