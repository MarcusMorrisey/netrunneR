#' Fetch NetrunnerDB reviews, rulings and decklists
#'
#' Fetches the NetrunnerDB reviews, rulings and decklist resources with a
#' req_user_agent() built from NRDB_CONTACT, one-to-two-second
#' req_throttle() pacing and req_retry() backoff, comparing the response
#' total, last_updated and version_number fields against the previous
#' release as warning-level checks.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "nrdb".
#' @param attempt_dir Character. Staging directory for this sync attempt.
#'
#' Delegates to run_decklist_sweep() (R/decklist-sweep.R) for the
#' date-range decklist sweep; abr and nrdb share the netrunneR_api_poll
#' S3 dispatch in R/fetch-api-poll.R but keep separate per-lineage
#' helper files because they carry different risk surfaces -- personal
#' data and pagination for abr, a date-range sweep for nrdb.
#' (ref: DL-005)
#'
#' @keywords internal
fetch_nrdb <- function(lineage, attempt_dir) {
  raw_dir <- file.path(attempt_dir, "raw")
  fs::dir_create(raw_dir)

  reviews <- nrdb_get(lineage$base_url, "/reviews")
  rulings <- nrdb_get(lineage$base_url, "/rulings")

  sweep <- run_decklist_sweep(lineage)

  jsonlite::write_json(reviews, file.path(raw_dir, "reviews.json"), auto_unbox = TRUE)
  jsonlite::write_json(rulings, file.path(raw_dir, "rulings.json"), auto_unbox = TRUE)

  previous <- previous_nrdb_manifest(lineage)
  comparison_checks <- list(
    compare_field(reviews$total, previous$reviews_total, "reviews_total"),
    compare_field(reviews$last_updated, previous$reviews_last_updated, "reviews_last_updated"),
    compare_field(reviews$version_number, previous$reviews_version_number, "reviews_version_number")
  )

  list(
    raw_dir = raw_dir,
    reviews = reviews,
    rulings = rulings,
    sweep = sweep,
    checks = comparison_checks,
    content_identity = digest::digest(list(reviews$total, rulings$total, sweep$sweep_end), algo = "sha256")
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

#' Warning-level field-by-field comparison against the previous release
#' @keywords internal
compare_field <- function(current, previous, label) {
  if (is.null(previous)) {
    return(list(check = label, status = "skip", message = "no previous release"))
  }
  status <- if (identical(current, previous)) "pass" else "warn"
  list(check = label, status = status, message = sprintf("previous=%s current=%s", previous, current))
}

#' @keywords internal
previous_nrdb_manifest <- function(lineage) {
  active <- tryCatch(resolve_release(lineage), error = function(e) NULL)
  if (is.null(active)) return(list())
  manifest_path <- file.path(active$release_dir, "manifest.json")
  if (!fs::file_exists(manifest_path)) return(list())
  jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
}
