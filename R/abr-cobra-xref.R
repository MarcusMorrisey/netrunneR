#' Human-verified abr/cobra tournament overrides
#'
#' The conservative fuzzy-match design for cross-referencing `abr` and
#' `cobra` tournament rows (see
#' `docs/netrunneR/plans/2026-09-03-abr-cobra-xref/plan.md`, homelab repo)
#' deliberately trades recall for precision: same-date-only requirement,
#' name-similarity >= 0.85, and mutual uniqueness reject any tournament
#' with more than one plausible candidate on either side. That is safe by
#' construction but leaves real matches on the table whenever names differ
#' too much for the similarity threshold (abbreviations, added "Online
#' Tournament"/"Store Champs" suffixes, reordering) or dates differ by a
#' day (near-midnight events landing on different calendar days per
#' source). A human manually reviewing `unmatched_tournaments.csv`
#' (exported from the same conservative logic) can resolve these with
#' full context the algorithm doesn't have -- this file is where that
#' judgment is recorded so it survives past one person's memory and one
#' spreadsheet, and so any future merge implementation is required to
#' honor it rather than re-deriving (and potentially re-missing) the same
#' calls. (ref: DL-046)
#'
#' These are overrides, not suggestions: a future merge step must treat
#' every row here as ground truth -- forced matches for
#' `abr_cobra_verified_matches.csv`, hard exclusions from the cobra side
#' of any merge for `abr_cobra_verified_deletes.csv` -- applied *before*
#' the algorithmic fuzzy tier runs, not merged/voted with it.
#'
#' @param path Character. Defaults to the packaged template.
#' @return A tibble with one row per human-verified (abr, cobra) match.
#' @keywords internal
read_abr_cobra_verified_matches <- function(path = system.file("extdata", "abr_cobra_verified_matches.csv",
                                                                package = "netrunneR")) {
  overrides <- readr::read_csv(
    path,
    col_types = readr::cols(
      abr_id = readr::col_character(),
      cobra_tournament_id = readr::col_character(),
      reason = readr::col_character(),
      verified_by = readr::col_character(),
      verified_at = readr::col_character()
    )
  )
  validate_abr_cobra_overrides(overrides, read_abr_cobra_verified_deletes())
  overrides
}

#' Human-verified cobra tournaments to exclude from any abr/cobra merge
#'
#' Companion to `read_abr_cobra_verified_matches()` -- see that function's
#' docs for why this file exists and how it must be applied. These are
#' rows a human confirmed are not real tournaments (test/junk data created
#' while exercising the Cobra platform itself), not rows that failed to
#' match; they must never appear in a merged feed under any source.
#'
#' @param path Character. Defaults to the packaged template.
#' @return A tibble with one row per human-verified exclusion.
#' @keywords internal
read_abr_cobra_verified_deletes <- function(path = system.file("extdata", "abr_cobra_verified_deletes.csv",
                                                                package = "netrunneR")) {
  readr::read_csv(
    path,
    col_types = readr::cols(
      source = readr::col_character(),
      source_id = readr::col_character(),
      reason = readr::col_character(),
      verified_by = readr::col_character(),
      verified_at = readr::col_character()
    )
  )
}

#' Fail closed if the human-verified override log contradicts itself
#'
#' Enforced at load time (every call to `read_abr_cobra_verified_matches()`)
#' and again in `tests/testthat/test-abr-cobra-xref.R` so a bad edit is
#' caught in CI, not silently fed into a future merge. Three invariants:
#' no id (on either side) claimed by more than one match row, no id
#' claimed by both a match and a delete, and every delete row is a cobra
#' row -- abr is community-maintained and out of scope for this repo to
#' mark for deletion.
#' @keywords internal
validate_abr_cobra_overrides <- function(matches, deletes) {
  dup_abr <- matches$abr_id[duplicated(matches$abr_id)]
  if (length(dup_abr) > 0) {
    stop("abr_cobra_verified_matches.csv: abr_id used in more than one row: ",
         paste(unique(dup_abr), collapse = ", "), call. = FALSE)
  }
  dup_cobra <- matches$cobra_tournament_id[duplicated(matches$cobra_tournament_id)]
  if (length(dup_cobra) > 0) {
    stop("abr_cobra_verified_matches.csv: cobra_tournament_id used in more than one row: ",
         paste(unique(dup_cobra), collapse = ", "), call. = FALSE)
  }

  non_cobra_deletes <- deletes$source[deletes$source != "cobra"]
  if (length(non_cobra_deletes) > 0) {
    stop("abr_cobra_verified_deletes.csv: only source == 'cobra' rows are supported, found: ",
         paste(unique(non_cobra_deletes), collapse = ", "), call. = FALSE)
  }

  deleted_cobra_ids <- deletes$source_id[deletes$source == "cobra"]
  conflict <- intersect(matches$cobra_tournament_id, deleted_cobra_ids)
  if (length(conflict) > 0) {
    stop("cobra_tournament_id present in both verified matches and verified deletes: ",
         paste(conflict, collapse = ", "), call. = FALSE)
  }

  invisible(TRUE)
}
