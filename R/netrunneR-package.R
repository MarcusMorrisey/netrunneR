#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom rlang abort
## usethis namespace: end
NULL

#' Fetch a lineage's raw data into a staging attempt directory
#'
#' S3 generic dispatched on the lineage's source_type class
#' (netrunneR_api_poll, netrunneR_git_mirror, netrunneR_web_archive).
#' Methods write only under attempt_dir and must route every byte derived
#' from an httr2_response through [capture_response_body()].
#' The netrunneR_api_poll method lives in R/fetch-api-poll.R and delegates
#' to per-lineage helpers in R/fetch-abr.R and R/fetch-nrdb.R. (ref: DL-005)
#'
#' @param lineage A lineage object from [lineage()] or [new_lineage()].
#' @param attempt_dir Character. Staging directory for this sync attempt.
#' @param ... Method-specific arguments.
#'
#' @return A list describing the staged raw content, at minimum
#'   content_identity and any lineage-specific provenance fields.
#' @export
fetch_lineage <- function(lineage, attempt_dir, ...) {
  UseMethod("fetch_lineage")
}

#' Build a lineage's processed release from staged raw content
#'
#' S3 generic dispatched on the lineage's source_type class. Methods read
#' from staged_raw and write the processed SQLite database and any derived
#' artifacts under the staging directory, returning the pieces
#' [validate_release()] and the manifest need.
#'
#' @param lineage A lineage object from [lineage()] or [new_lineage()].
#' @param staged_raw The value returned by the matching [fetch_lineage()] method.
#' @param ... Method-specific arguments.
#'
#' @return A list describing the built release, at minimum build_revision
#'   and the path to the processed SQLite database.
#' @export
build_lineage <- function(lineage, staged_raw, ...) {
  UseMethod("build_lineage")
}
