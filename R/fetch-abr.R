#' Fetch ABR tournaments, entries, videos and upcoming events
#'
#' Fetches the ABR tournaments results pages, per-tournament entries, the
#' bulk videos endpoint and the upcoming endpoint sequentially with
#' two-second req_throttle() pacing and a hard stop on any 5xx. Uses a
#' fresh req_options cookie jar per request deleted immediately after,
#' sends no conditional headers, derives the page count from
#' tournament_count on the first element rather than looping until empty,
#' and enforces the 500-row limit cap with a hard stopifnot().
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

  # The live endpoint's response body is a bare JSON array of tournament
  # objects -- there is no {"results": [...], "tournament_count": N}
  # wrapper -- and only the array's first element carries a
  # tournament_count field at all; every other element's tournament_count
  # is null. jsonlite::fromJSON(simplifyVector = TRUE) auto-coerces that
  # array into a data.frame, so first_page$tournament_count is a whole
  # column (mostly NA past row 1), not a scalar: `NA >= 0` is NA, and
  # stopifnot() on a length>1 vector containing an NA both fail in ways
  # that only surface once real (not fixture-shaped) data is fetched.
  first_page <- abr_get(lineage$base_url, "/tournaments/results", list(limit = page_limit, offset = 0))
  tournament_count <- first_page$tournament_count[1]
  stopifnot(is.numeric(tournament_count), tournament_count >= 0)

  pages <- list(first_page)
  offset <- page_limit
  while (offset < tournament_count) {
    pages[[length(pages) + 1]] <- abr_get(lineage$base_url, "/tournaments/results", list(limit = page_limit, offset = offset))
    offset <- offset + page_limit
  }

  # Each page IS the tournaments data.frame directly (see the bare-array
  # note above) -- there is no $results field to index into. p$results
  # previously returned NULL on every page, silently collapsing
  # `tournaments` to zero rows on every real (non-fixture) run.
  tournaments <- purrr::map_dfr(pages, tibble::as_tibble)
  if (nrow(tournaments) == 0) {
    # An empty `results` page collapses to a zero-column tibble via
    # as_tibble(list()), leaving no `id` column for the lookups below and
    # no ABR_TOURNAMENT_ALLOWLIST columns for build_abr()'s all_of() select
    # to find. Reintroduce the expected shape with zero rows so a
    # legitimately empty page doesn't crash downstream.
    tournaments <- tibble::as_tibble(stats::setNames(
      rep(list(character(0)), length(ABR_TOURNAMENT_ALLOWLIST)), ABR_TOURNAMENT_ALLOWLIST
    ))
  }

  entries <- purrr::map(tournaments$id, function(id) abr_get(lineage$base_url, "/entries", list(id = id)))
  # /videos is a single bulk call covering every tournament's videos, not
  # a per-tournament endpoint -- fetched once regardless of how many
  # tournaments were returned above.
  videos <- abr_get(lineage$base_url, "/videos")
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
  # httr2 auto-throws its own httr2_http_* error on any non-2xx status by
  # default, which would bypass the netrunneR_abr_5xx classification below
  # entirely -- disable that so req_perform() always returns a response.
  req <- httr2::req_error(req, is_error = function(resp) FALSE)

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
