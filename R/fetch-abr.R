#' Fetch ABR tournaments, entries, videos and upcoming events
#'
#' Fetches the ABR tournaments results pages, per-tournament entries,
#' videos and upcoming endpoints sequentially with two-second
#' req_throttle() pacing and a hard stop on any 5xx. Uses a fresh
#' req_options cookie jar per request deleted immediately after, sends no
#' conditional headers, derives the page count from tournament_count on
#' the first element rather than looping until empty, and enforces the
#' 500-row limit cap with a hard stopifnot().
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "abr".
#' @param attempt_dir Character. Staging directory for this sync attempt.
#'
#' raw_dir is created 0700 (owner-only): it holds unprocessed ABR
#' provenance data that has not yet passed the two-layer allowlist and
#' deny-pattern check in build_abr(). (ref: DL-002)
#'
#' @keywords internal
fetch_abr <- function(lineage, attempt_dir) {
  raw_dir <- file.path(attempt_dir, "raw")
  fs::dir_create(raw_dir, mode = "0700")

  page_limit <- 500L
  stopifnot(page_limit <= 500L)

  first_page <- abr_get(lineage$base_url, "/tournaments/results", list(limit = page_limit, offset = 0))
  tournament_count <- first_page$tournament_count
  stopifnot(is.numeric(tournament_count), tournament_count >= 0)

  pages <- list(first_page)
  offset <- page_limit
  while (offset < tournament_count) {
    pages[[length(pages) + 1]] <- abr_get(lineage$base_url, "/tournaments/results", list(limit = page_limit, offset = offset))
    offset <- offset + page_limit
  }

  tournaments <- purrr::map_dfr(pages, function(p) tibble::as_tibble(p$results))

  entries <- purrr::map(tournaments$id, function(id) abr_get(lineage$base_url, sprintf("/tournaments/%s/entries", id)))
  videos <- purrr::map(tournaments$id, function(id) abr_get(lineage$base_url, sprintf("/tournaments/%s/videos", id)))
  upcoming <- abr_get(lineage$base_url, "/tournaments/upcoming")

  write_json_raw(raw_dir, "tournaments.json", tournaments)
  write_json_raw(raw_dir, "entries.json", entries)
  write_json_raw(raw_dir, "videos.json", videos)
  write_json_raw(raw_dir, "upcoming.json", upcoming)

  list(
    raw_dir = raw_dir,
    tournament_count = tournament_count,
    tournaments = tournaments,
    entries = entries,
    videos = videos,
    upcoming = upcoming,
    content_identity = digest::digest(list(tournaments$id, tournament_count), algo = "sha256")
  )
}

#' Issue one paced, cookie-isolated GET against the ABR API
#'
#' Every request gets a fresh req_options cookie jar deleted immediately
#' after the request completes, and sends no conditional headers, so no
#' Set-Cookie or ETag/Last-Modified value from one request can leak into a
#' later request or onto disk. A hard stop on any 5xx protects a
#' volunteer-run server from a retry storm during an upstream outage.
#'
#' Invariant: the response body reaches disk only through
#' capture_response_body() below -- this helper never calls writeBin(),
#' writeLines() or jsonlite::write_json() on the httr2 response itself.
#' (ref: DL-005)
#' @keywords internal
abr_get <- function(base_url, path, query = list()) {
  jar <- tempfile(fileext = ".sqlite")
  on.exit(unlink(jar), add = TRUE)

  req <- httr2::request(paste0(base_url, path))
  req <- httr2::req_url_query(req, !!!query)
  req <- httr2::req_options(req, cookiejar = jar, cookiefile = jar)
  req <- httr2::req_throttle(req, rate = 1 / 2)

  resp <- httr2::req_perform(req)
  if (httr2::resp_status(resp) >= 500) {
    rlang::abort(sprintf("ABR returned %d for %s; hard stop", httr2::resp_status(resp), path), class = "netrunneR_abr_5xx")
  }

  body <- capture_response_body(resp, as = "string")
  jsonlite::fromJSON(body, simplifyVector = TRUE)
}

#' @keywords internal
write_json_raw <- function(raw_dir, filename, obj) {
  jsonlite::write_json(obj, file.path(raw_dir, filename), auto_unbox = TRUE)
  Sys.chmod(file.path(raw_dir, filename), mode = "0600")
}
