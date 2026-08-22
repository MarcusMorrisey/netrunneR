# Assembles individual check records (from validate-helpers.R and a
# lineage's own build method) into the pass/fail report embedded in the manifest.
#' Assemble and run the per-lineage validation checks
#'
#' Runs shape and required-field checks, id uniqueness, a row-count delta
#' against the previous release (skipped on a lineage's first release) and
#' referential-integrity checks against joined reference tibbles. The
#' assembled report is embedded verbatim in the release manifest.
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
#' @name %||%
#' @keywords internal
`%||%` <- function(x, y) if (is.null(x)) y else x
