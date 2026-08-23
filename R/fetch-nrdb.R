#' Fetch NetrunnerDB reviews and rulings
#'
#' Fetches the NetrunnerDB reviews and rulings resources with a
#' req_user_agent() built from NRDB_CONTACT, one-to-two-second
#' req_throttle() pacing and req_retry() backoff, running a per-attempt
#' shape check against each response's data envelope.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "nrdb".
#' @param attempt_dir Character. Staging directory for this sync attempt.
#'
#' @keywords internal
fetch_nrdb <- function(lineage, attempt_dir) {
  raw_dir <- file.path(attempt_dir, "raw")
  fs::dir_create(raw_dir)

  reviews <- nrdb_get(lineage$base_url, "/reviews")
  rulings <- nrdb_get(lineage$base_url, "/rulings")

  jsonlite::write_json(reviews, file.path(raw_dir, "reviews.json"), auto_unbox = TRUE)
  jsonlite::write_json(rulings, file.path(raw_dir, "rulings.json"), auto_unbox = TRUE)

  # The real NetrunnerDB /reviews and /rulings envelope is `{"data": [...]}`
  # only -- no total, last_updated or version_number field exists at the
  # top level (verified live against netrunnerdb.com/api/2.0/public this
  # session). The previous field-by-field comparison against those
  # nonexistent fields was already inert before this fix: write_manifest()
  # (R/sync.R) only ever persists release_id, lineage, content_identity,
  # build_revision, validate_report and promoted_at onto manifest.json, so
  # previous$reviews_total/last_updated/version_number were always NULL
  # and compare_field() always fell back to its "skip" branch. Rather than
  # invent a fake historical comparison against state that was never
  # reliably persisted, this now runs a per-attempt shape check -- does
  # the envelope actually carry a `data` list -- which is the one real,
  # honestly-derived signal available here. The real content-change
  # signal for nrdb already exists at the sync layer, where
  # no_op_change() (R/sync.R) diffs content_identity against the active
  # release.
  comparison_checks <- list(
    compare_shape(reviews, "reviews"),
    compare_shape(rulings, "rulings")
  )

  list(
    raw_dir = raw_dir,
    reviews = reviews,
    rulings = rulings,
    checks = comparison_checks,
    content_identity = digest::digest(
      list(NROW(reviews$data), NROW(rulings$data)),
      algo = "sha256"
    )
  )
}

#' Issue a paced NetrunnerDB request identified by NRDB_CONTACT
#'
#' Invariant: the response body reaches disk only through
#' capture_response_body() below -- this helper never calls writeBin(),
#' writeLines() or jsonlite::write_json() on the httr2 response itself.
#' (ref: DL-005)
#' @keywords internal
nrdb_get <- function(base_url, path, query = list()) {
  contact <- Sys.getenv("NRDB_CONTACT")
  req <- httr2::request(paste0(base_url, path))
  req <- httr2::req_url_query(req, !!!query)
  req <- httr2::req_user_agent(req, sprintf("netrunneR-mirror (%s)", contact))
  req <- httr2::req_throttle(req, rate = 1 / 1.5)
  req <- httr2::req_retry(req, max_tries = 5)

  resp <- httr2::req_perform(req)
  body <- capture_response_body(resp, as = "string")
  jsonlite::fromJSON(body, simplifyVector = TRUE)
}

#' Per-attempt shape check on a NetrunnerDB response envelope
#'
#' Verifies the response actually carries a `data` list, since that is
#' the only field the real API guarantees at the top level. A fail-status
#' result blocks promotion via validate_release()'s fail gate (R/validate.R).
#' @keywords internal
compare_shape <- function(resp, label) {
  ok <- is.list(resp) && "data" %in% names(resp) && is.list(resp$data)
  list(
    check = sprintf("%s_shape", label),
    status = if (ok) "pass" else "fail",
    message = sprintf("data field present=%s rows=%s", ok, if (ok) NROW(resp$data) else NA)
  )
}
