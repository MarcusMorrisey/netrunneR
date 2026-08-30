# Individual validation-check primitives combined by validate_release()
# into one report; each returns a {check, status, message} record.
#' Check that every value of a column falls in an allowed set
#' @param df A data frame.
#' @param column Character. Name of the column to check.
#' @param allowed Vector of values the column is allowed to contain.
#' @return A check-result list with `check`, `status`, and `message`.
#' @export
check_col_vals_in_set <- function(df, column, allowed) {
  bad <- setdiff(unique(df[[column]]), allowed)
  status <- if (length(bad) == 0) "pass" else "fail"
  list(
    check = sprintf("col_vals_in_set:%s", column),
    status = status,
    message = if (status == "fail") sprintf("Unexpected values in %s: %s", column, paste(bad, collapse = ", ")) else "ok"
  )
}

#' Check that rows are distinct on one or more key columns
#' @param df A data frame.
#' @param keys Character vector of one or more column names forming the
#'   uniqueness key.
#' @return A check-result list with `check`, `status`, and `message`.
#' @export
check_rows_distinct <- function(df, keys) {
  n_dup <- sum(duplicated(df[keys]))
  status <- if (n_dup == 0) "pass" else "fail"
  list(
    check = sprintf("rows_distinct:%s", paste(keys, collapse = "+")),
    status = status,
    message = if (status == "fail") sprintf("%d duplicate rows on %s", n_dup, paste(keys, collapse = "+")) else "ok"
  )
}

#' Compare row count against the previous release, warning past a delta threshold
#'
#' Skipped (status "skip") on a lineage's first release, when there is no
#' previous row count to compare against.
#'
#' @details
#' A large drop warns rather than blocks because row count is only a proxy
#' for lineage health, not a direct measure of it: a legitimate upstream
#' change -- a source pruning stale or superseded entries, a schema or
#' allowlist change that reshapes which rows survive -- can produce a big,
#' genuine drop that isn't a build defect. Blocking promotion on every such
#' drop would stall a lineage indefinitely until a human intervenes anyway,
#' so the drop is surfaced as a warning for a human to eyeball instead of
#' failing the build outright.
#'
#' This function is currently unused: no `build_lineage.*()` method calls
#' it. It is standalone infrastructure, presumably intended to be wired
#' into a future lineage's validate step once a suitable `max_pct_drop`
#' threshold is chosen for that lineage; until then it provides no actual
#' protection.
#' @param current_n Integer. Row count in the current release.
#' @param previous_n Integer or NULL/NA. Row count in the previous release,
#'   if any.
#' @param max_pct_drop Numeric. Fractional drop (0-1) past which status is
#'   "warn" rather than "pass".
#' @return A check-result list with `check`, `status`, and `message`.
#' @export
check_row_count_delta <- function(current_n, previous_n, max_pct_drop = 0.5) {
  if (is.null(previous_n) || is.na(previous_n) || previous_n == 0) {
    return(list(check = "row_count_delta", status = "skip", message = "no previous release"))
  }
  pct_drop <- (previous_n - current_n) / previous_n
  status <- if (pct_drop > max_pct_drop) "warn" else "pass"
  list(
    check = "row_count_delta",
    status = status,
    message = sprintf("previous=%d current=%d delta_pct=%.1f%%", previous_n, current_n, pct_drop * 100)
  )
}

#' Default deny pattern for personal-data column names
#'
#' Second, independent enforcement layer alongside the upstream dplyr::select() allowlist.
#' Used by check_deny_pattern() as an enforcement layer independent of any
#' upstream dplyr::select() allowlist -- never a restatement of it.
#' @name DEFAULT_DENY_PATTERN
#' @export
DEFAULT_DENY_PATTERN <- "(?i)(contact|e[-_]?mail|player[-_]?name|user[-_]?name|creator|importer|uploader|organizer|address|lat(itude)?|lon(gitude)?|\\bbio\\b|notes?|description)"

#' Scan column names against a deny-pattern regex before any write
#'
#' A fail-closed check independent of any upstream select() allowlist: a
#' match returns status "fail" so the caller must abort the write rather
#' than silently dropping the offending columns.
#'
#' @param df A data frame or tibble.
#' @param deny_pattern Character. A regular expression matched against column names.
#'
#' @export
check_deny_pattern <- function(df, deny_pattern = DEFAULT_DENY_PATTERN) {
  hits <- grep(deny_pattern, names(df), perl = TRUE, value = TRUE)
  status <- if (length(hits) == 0) "pass" else "fail"
  list(
    check = "deny_pattern",
    status = status,
    message = if (status == "fail") sprintf("Denied column names present: %s", paste(hits, collapse = ", ")) else "ok"
  )
}
#' ABR-specific deny pattern for personal-data column names
#'
#' A specialization of DEFAULT_DENY_PATTERN naming the exact ABR upstream
#' field shapes (player/user/creator/importer/uploader/organizer names or
#' handles, contact, addresses, coordinates, free-text bio/notes fields)
#' this lineage must exclude, run as an independent second enforcement
#' layer separate from and after the dplyr::select() allowlist in
#' build_abr(), never a restatement of it.
#'
#' Both this pattern and the ABR_TOURNAMENT_ALLOWLIST select() in
#' build_abr() must pass before any DBI::dbWriteTable() call -- each is
#' a complete, independent fail-closed check on its own, not a partial
#' contribution to one combined check. (ref: DL-002)
#'
#' THE COORDINATE ALTERNATIVES WERE REMOVED, deliberately. This pattern
#' once rejected `lat(itude)?` and `lon(gitude)?`, which is why
#' `location_lat` was blocked here as well as omitted from the
#' allowlist. Venue coordinates are now admitted -- see
#' ABR_TOURNAMENT_ALLOWLIST for the reasoning, which is that a venue is
#' a property of an event and not of a person.
#'
#' Note what the old pattern actually did, because it flatters itself:
#' `lat(itude)?` matched `location_lat`, but `lon(gitude)?` never
#' matched `location_lng` -- there is no "lon" in that name. Layer two
#' was therefore blocking one coordinate and missing the other, and only
#' the allowlist was keeping `location_lng` out. A second layer that
#' half-works is worth knowing about; it is recorded here rather than
#' quietly tidied away.
#'
#' DEFAULT_DENY_PATTERN above is UNCHANGED and still rejects both
#' coordinate spellings. This widening is scoped to the abr lineage,
#' where the venue argument applies, and to nothing else.
#' @export
ABR_DENY_PATTERN <- "(?i)(contact|e[-_]?mail|player[-_]?(name|handle)|user[-_]?(name|handle)|creator|importer|uploader|organizer|address|\\bbio\\b|notes?|description)"
