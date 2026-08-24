#' Fetch an api-poll lineage's raw data
#'
#' The single S3 method for api-poll lineages: delegates the request
#' sequence to the per-lineage internal helper for abr or nrdb. Each of
#' those helpers derives its own httr2::req_throttle() rate from the
#' lineage's own pacing policy (lineage$pacing) via pacing_rate() below,
#' so .LINEAGE_REGISTRY's pacing entry (R/lineage.R) is the real,
#' honored source of truth for that lineage's request pacing rather than
#' an independently hardcoded rate. Every byte it hands onward comes
#' from capture_response_body().
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

#' Convert a pacing policy's min/max delay range to a req_throttle() rate
#'
#' httr2::req_throttle() takes a requests-per-second rate, not a delay
#' range, so this converts a lineage's pacing = list(min_delay_s,
#' max_delay_s) into a single fixed rate of one request per the mean of
#' the two bounds -- the conservative middle of the declared range. This
#' is the one place a lineage's pacing policy (as set in
#' .LINEAGE_REGISTRY, R/lineage.R) becomes an actual throttle rate, so
#' nrdb_get() (R/fetch-nrdb.R) and abr_get() (R/fetch-abr.R) both call
#' this rather than hardcoding their own rate.
#'
#' @param pacing A list with numeric min_delay_s and max_delay_s fields,
#'   as carried on a lineage object (lineage$pacing).
#'
#' @return A numeric requests-per-second rate suitable for
#'   httr2::req_throttle(rate = ...).
#' @keywords internal
pacing_rate <- function(pacing) {
  stopifnot(
    is.list(pacing),
    is.numeric(pacing$min_delay_s), is.numeric(pacing$max_delay_s),
    pacing$min_delay_s > 0, pacing$max_delay_s > 0
  )
  1 / mean(c(pacing$min_delay_s, pacing$max_delay_s))
}
