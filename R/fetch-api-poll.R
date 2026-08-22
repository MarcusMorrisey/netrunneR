#' Fetch an api-poll lineage's raw data
#'
#' The single S3 method for api-poll lineages: applies the lineage's own
#' pacing and user-agent policy, then delegates the request sequence to
#' the per-lineage internal helper for abr or nrdb. Every byte it hands
#' onward comes from capture_response_body().
#'
#' Invariant: capture_response_body() (R/capture.R) is the only function
#' in the package permitted to write httr2 response bytes to disk. No
#' fetch_lineage() method -- including this dispatcher and the
#' abr/nrdb helpers it delegates to -- may write a response or its
#' headers to disk directly.
#'
#' @param lineage A lineage object of class netrunneR_api_poll.
#' @param attempt_dir Character. Staging directory for this sync attempt.
#' @param ... Ignored.
#'
#' @export
fetch_lineage.netrunneR_api_poll <- function(lineage, attempt_dir, ...) {
  if (identical(lineage$name, "abr")) {
    fetch_abr(lineage, attempt_dir)
  } else if (identical(lineage$name, "nrdb")) {
    fetch_nrdb(lineage, attempt_dir)
  } else {
    rlang::abort(sprintf("No api-poll fetch helper for lineage '%s'", lineage$name), class = "netrunneR_no_fetch_method")
  }
}
