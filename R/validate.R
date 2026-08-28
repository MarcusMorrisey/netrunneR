# Assembles individual check records (from validate-helpers.R and a
# lineage's own build method) into the pass/fail report embedded in the manifest.
#' Assemble the per-lineage validation checks into a pass/fail report
#'
#' Runs no checks itself, despite an earlier title saying it did. The
#' individual records -- shape and required-field checks, id uniqueness,
#' the row-count delta against the previous release, referential-integrity
#' checks -- are produced by validate-helpers.R and by the lineage's own
#' build method, and arrive here already computed. This function
#' concatenates them, fails the report if any single record failed, and
#' returns it for verbatim embedding in the release manifest.
#'
#' Enforcement is the caller's: run_sync() (R/sync.R) is what refuses to
#' promote on a failing report.
#'
#' @param lineage A lineage object.
#' @param built The value returned by the matching build_lineage() method.
#' @param checks A list of additional {check, status, message} records supplied
#'   by the lineage's build method (e.g. deny-pattern, monotonicity checks).
#'
#' @return A list with `status` ("pass" or "fail") and `checks`, the full list
#'   of individual check records.
#' @export
validate_release <- function(lineage, built, checks = list()) {
  all_checks <- c(built$checks %||% list(), checks)

  has_fail <- any(vapply(all_checks, function(x) identical(x$status, "fail"), logical(1)))

  list(
    status = if (has_fail) "fail" else "pass",
    checks = all_checks
  )
}

#' Null-coalescing helper used internally across validate.R for optional args.
#' @name grapes-or-or-grapes
#' @aliases %||%
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
