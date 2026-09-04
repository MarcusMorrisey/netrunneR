#' Fetch Cobra tournament data
#'
#' Fetches from NSG's official tournament platform at
#' https://tournaments.nullsignal.games/, a source with no complete
#' historical index endpoint -- only 12 public "type" listing pages
#' (currently-listed tournaments per type) and per-tournament JSON
#' endpoints. Unlike fetch_abr(), which learns its complete id set from
#' one paginated API call, fetch_cobra() combines three id-discovery
#' mechanisms on every call, regardless of mode: (1) scraping the 12 type
#' pages for currently-listed ids, (2) probing a short tail of ids above
#' the highest id ever seen, for newly created tournaments, and (3)
#' advancing a bounded batch of a persistent low-to-high id walk to
#' gradually backfill tournaments no longer listed on any type page. mode
#' never reaches fetch_lineage() in this codebase (see run_sync(),
#' R/sync.R -- mode only gates no_op_change()'s short-circuit and
#' rollback dispatch), so the historical walk advances by a fixed batch
#' on every fetch call, scheduled or backfill alike -- the same
#' "cheap to resume, mode-independent" idiom fetch_abr() already uses for
#' run_abr_backfill() (R/abr-backfill.R).
#'
#' Every tournament bundle (pairings/standings/id_and_faction/cut_conversion)
#' is written into a content-addressed object pool at
#' lineage$store_root/objects/<id>.json, checkpointed in a small tibble
#' persisted beside the pool, so an interrupted crawl resumes rather than
#' refetches. Tournaments found via the type pages or the new-id tail are
#' always refreshed (their pairings/standings can change while an event
#' is live); tournaments found only via the historical batch walk are
#' fetched once and treated as immutable thereafter. Every attempt
#' rebuilds its tables from the entire pool, not just the ids touched
#' this run, so coverage grows monotonically across runs.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "cobra".
#' @param attempt_dir Character. Staging directory for this sync attempt.
#'
#' raw_dir and the object pool are created 0700 (owner-only): pairings and
#' standings responses carry real player display names before
#' build_cobra()'s allowlist/deny-pattern layers strip them. (ref: DL-002)
#'
#' @return A list including `raw_dir`, `bundles` (a named list of parsed
#'   per-tournament bundles keyed by tournament id, covering the whole
#'   pool), `recent_index`, `checks` and `content_identity`.
#'
#' @keywords internal
fetch_cobra <- function(lineage, attempt_dir) {
  raw_dir <- file.path(attempt_dir, "raw")
  fs::dir_create(raw_dir, mode = "0700")

  pool_dir <- file.path(lineage$store_root, "objects")
  fs::dir_create(pool_dir, mode = "0700")

  checkpoint_path <- file.path(lineage$store_root, "cobra-backfill-checkpoint.rds")
  checkpoint <- read_cobra_checkpoint(checkpoint_path)

  crawl_state_path <- file.path(lineage$store_root, "cobra-crawl-state.json")
  crawl_state <- read_cobra_crawl_state(crawl_state_path)

  recent_index <- discover_cobra_recent_index(lineage)
  recent_ids <- sort(unique(recent_index$tournament_id))
  crawl_state$max_known_id <- max(c(crawl_state$max_known_id, recent_ids, 0L), na.rm = TRUE)

  tail_result <- probe_cobra_new_tail(lineage, crawl_state$max_known_id + 1L)
  crawl_state$max_known_id <- max(crawl_state$max_known_id, tail_result$max_checked_id, na.rm = TRUE)

  refresh_ids <- sort(unique(c(recent_ids, tail_result$found_ids)))
  for (id in refresh_ids) {
    bundle <- scrape_cobra_bundle(lineage, id)
    checkpoint <- record_cobra_bundle(pool_dir, checkpoint, bundle)
  }

  backfill_result <- advance_cobra_backfill(lineage, checkpoint, crawl_state, pool_dir)
  checkpoint <- backfill_result$checkpoint
  crawl_state <- backfill_result$crawl_state

  saveRDS(checkpoint, checkpoint_path)
  write_cobra_crawl_state(crawl_state, crawl_state_path)

  resolved_ids <- checkpoint$tournament_id[checkpoint$exists]
  bundles <- stats::setNames(
    lapply(resolved_ids, function(id) read_cobra_object(pool_dir, id)),
    resolved_ids
  )

  write_json_raw(raw_dir, "recent_index.json", recent_index)

  checks <- list(
    check_cobra_pool_nonempty(bundles),
    check_cobra_bundle_shape(bundles)
  )

  list(
    raw_dir = raw_dir,
    pool_dir = pool_dir,
    bundles = bundles,
    recent_index = recent_index,
    checks = checks,
    content_identity = cobra_content_identity(pool_dir, resolved_ids)
  )
}

#' Number of public tournament-type listing pages Cobra exposes
#' @keywords internal
COBRA_TYPE_ID_COUNT <- 12L

#' Consecutive misses above the highest known id before the new-tail
#' probe gives up for this run.
#' @keywords internal
COBRA_TAIL_MISS_LIMIT <- 25L

#' Ids advanced per fetch call in the historical low-to-high backfill walk.
#' @keywords internal
COBRA_BACKFILL_BATCH_SIZE <- 200L

#' Issue one paced, retried GET against the Cobra tournament site
#'
#' The req_throttle() rate is derived from lineage$pacing via
#' pacing_rate() (R/fetch-api-poll.R), matching abr_get()/nrdb_get()'s
#' discipline of never hardcoding a rate independent of
#' .LINEAGE_REGISTRY. A 401/403/404/406 is treated as "this tournament id
#' or page does not exist" rather than an error, matching the public,
#' unauthenticated crawl this lineage runs. A 429 or 5xx is retried with
#' capped exponential backoff up to max_retries; a 5xx or 429 exhausting
#' retries aborts the whole attempt with a netrunneR_cobra_5xx condition,
#' the same fail-closed hard stop abr_get() uses to protect a
#' community-run server from a retry storm.
#'
#' Invariant: the response body reaches disk only through
#' capture_response_body() below -- this helper never calls writeBin(),
#' writeLines() or jsonlite::write_json() on the httr2 response itself.
#' (ref: DL-005)
#'
#' @param lineage A lineage object (or any list carrying base_url and
#'   pacing) identifying the request's base_url and pacing policy.
#' @param path Character. Path appended to lineage$base_url.
#' @param query Named list. Query parameters.
#' @param as Character. "json" to parse the body as JSON, "html" to
#'   return the raw response text.
#' @param max_retries Integer. Attempts before a 429/5xx aborts.
#'
#' @return A list with `ok` (logical), `status` (integer) and `body`
#'   (NULL when `ok` is FALSE).
#'
#' @keywords internal
cobra_get <- function(lineage, path, query = list(), as = c("json", "html"), max_retries = 4L) {
  as <- match.arg(as)

  for (attempt in seq_len(max_retries)) {
    req <- httr2::request(paste0(lineage$base_url, path))
    req <- httr2::req_url_query(req, !!!query)
    req <- httr2::req_throttle(req, rate = pacing_rate(lineage$pacing))
    # httr2 auto-throws its own httr2_http_* error on any non-2xx status
    # by default, which would bypass the ok=FALSE/retry handling below
    # entirely -- disable that so req_perform() always returns a response.
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    # A missing tournament id redirects (302) to /error rather than
    # 404ing -- httr2 follows redirects by default, which would silently
    # turn that into a 200 whose body is the site's HTML error shell,
    # not JSON, and jsonlite::fromJSON() on that crashes with a lexical
    # error rather than a clean not-found. Disabling follow makes the
    # redirect status visible so it can be treated as not-found below,
    # the same way a real 404 already is. Confirmed live against
    # /tournaments/99999/rounds/pairings_data, 2026-09-04.
    req <- httr2::req_options(req, followlocation = 0L)

    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)

    if (status %in% c(301L, 302L, 303L, 307L, 308L, 401L, 403L, 404L, 406L)) {
      return(list(ok = FALSE, status = as.integer(status), body = NULL))
    }

    if (status >= 200 && status < 300) {
      raw_body <- capture_response_body(resp, as = "string")
      # simplifyVector = FALSE, not TRUE: Cobra's per-tournament envelopes
      # are deeply and irregularly nested (stages -> rounds -> pairings,
      # each with heterogeneous per-object fields), so this preserves the
      # same list-of-lists shape the flatten_cobra_*() helpers
      # (R/build-cobra.R) walk with $ access and for-loops -- unlike
      # abr_get()/nrdb_get()'s simplifyVector = TRUE, which suits their
      # flat, uniform arrays-of-objects instead.
      body <- if (identical(as, "json")) jsonlite::fromJSON(raw_body, simplifyVector = FALSE) else raw_body
      return(list(ok = TRUE, status = as.integer(status), body = body))
    }

    if (status %in% c(429L, 500L, 502L, 503L, 504L) && attempt < max_retries) {
      Sys.sleep(min(30, 2^(attempt - 1)))
      next
    }

    rlang::abort(
      sprintf("Cobra returned %d for %s after %d attempt(s); hard stop", status, path, attempt),
      class = "netrunneR_cobra_5xx"
    )
  }

  rlang::abort(sprintf("Cobra request exhausted retries for %s", path), class = "netrunneR_cobra_5xx")
}

#' Scrape the 12 public type pages for currently-listed tournament ids
#' @keywords internal
discover_cobra_recent_index <- function(lineage) {
  discovered_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  rows <- list()

  for (type_id in seq_len(COBRA_TYPE_ID_COUNT)) {
    result <- cobra_get(lineage, sprintf("/tournaments/type/%d", type_id), as = "html")
    if (!isTRUE(result$ok)) next

    ids <- unique(as.integer(gsub(
      "\\D", "",
      regmatches(result$body, gregexpr("/tournaments/[0-9]+", result$body, perl = TRUE))[[1]]
    )))
    if (length(ids) == 0) next

    rows[[length(rows) + 1L]] <- tibble::tibble(
      tournament_id = ids, tournament_type_id = type_id, discovered_at = discovered_at
    )
  }

  if (length(rows) == 0) {
    return(tibble::tibble(tournament_id = integer(0), tournament_type_id = integer(0), discovered_at = character(0)))
  }

  dplyr::bind_rows(rows)
}

#' Probe upward from start_id for newly assigned tournament ids
#'
#' Stops once COBRA_TAIL_MISS_LIMIT consecutive ids do not exist, so the
#' per-run cost stays bounded independent of how far the true upper bound
#' has moved.
#' @keywords internal
probe_cobra_new_tail <- function(lineage, start_id) {
  found_ids <- integer(0)
  misses <- 0L
  current_id <- start_id

  while (misses < COBRA_TAIL_MISS_LIMIT) {
    bundle <- scrape_cobra_bundle(lineage, current_id)
    if (isTRUE(bundle$exists)) {
      found_ids <- c(found_ids, current_id)
      misses <- 0L
    } else {
      misses <- misses + 1L
    }
    current_id <- current_id + 1L
  }

  list(found_ids = found_ids, max_checked_id = current_id - 1L)
}

#' Fetch one tournament's full bundle of Cobra endpoints
#'
#' The pairings endpoint is authoritative for whether the tournament
#' exists at all (its own 404 means the id is unassigned); the other
#' three endpoints are fetched only once pairings confirms existence, and
#' a 404 on any of them is treated as "not published for this tournament"
#' rather than aborting the bundle.
#' @keywords internal
scrape_cobra_bundle <- function(lineage, tournament_id) {
  pairings <- cobra_get(lineage, sprintf("/tournaments/%d/rounds/pairings_data", tournament_id))
  if (!isTRUE(pairings$ok)) {
    return(list(exists = FALSE, tournament_id = tournament_id))
  }

  standings <- cobra_get(lineage, sprintf("/tournaments/%d/players/standings_data", tournament_id))
  id_and_faction <- cobra_get(lineage, sprintf("/tournaments/%d/id_and_faction_data", tournament_id))
  cut_conversion <- cobra_get(lineage, sprintf("/tournaments/%d/cut_conversion_rates", tournament_id))

  list(
    exists = TRUE,
    tournament_id = tournament_id,
    pairings_data = pairings$body,
    standings_data = if (isTRUE(standings$ok)) standings$body else NULL,
    id_and_faction_data = if (isTRUE(id_and_faction$ok)) id_and_faction$body else NULL,
    cut_conversion_rates = if (isTRUE(cut_conversion$ok)) cut_conversion$body else NULL
  )
}

#' Read the cobra backfill checkpoint tibble, seeding it if absent
#' @keywords internal
read_cobra_checkpoint <- function(checkpoint_path) {
  if (fs::file_exists(checkpoint_path)) {
    return(readRDS(checkpoint_path))
  }
  tibble::tibble(tournament_id = character(0), exists = logical(0), last_fetched_at = character(0))
}

#' Read the cobra crawl state (max known id, backfill pointer), seeding
#' defaults if absent. Persisted as JSON rather than RDS so it stays
#' human-inspectable on disk.
#' @keywords internal
read_cobra_crawl_state <- function(crawl_state_path) {
  if (!fs::file_exists(crawl_state_path)) {
    return(list(max_known_id = 0L, backfill_next_id = 1L, backfill_complete = FALSE))
  }
  state <- jsonlite::fromJSON(crawl_state_path, simplifyVector = TRUE)
  list(
    max_known_id = as.integer(state$max_known_id %||% 0L),
    backfill_next_id = as.integer(state$backfill_next_id %||% 1L),
    backfill_complete = isTRUE(state$backfill_complete)
  )
}

#' @keywords internal
write_cobra_crawl_state <- function(state, crawl_state_path) {
  jsonlite::write_json(state, crawl_state_path, auto_unbox = TRUE, pretty = TRUE)
  Sys.chmod(crawl_state_path, mode = "0600")
}

#' Persist one bundle's outcome to the pool and checkpoint
#'
#' A bundle with exists = TRUE is written to the object pool (0600) and
#' upserted into checkpoint with exists = TRUE; exists = FALSE upserts a
#' settled, dataless checkpoint row so the id is never re-probed. Used for
#' both the always-refreshed recent/tail ids and the once-only historical
#' backfill walk.
#' @keywords internal
record_cobra_bundle <- function(pool_dir, checkpoint, bundle) {
  today <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  id <- as.character(bundle$tournament_id)

  if (isTRUE(bundle$exists)) {
    write_cobra_object(pool_dir, id, bundle)
  }

  dplyr::rows_upsert(
    checkpoint,
    tibble::tibble(tournament_id = id, exists = isTRUE(bundle$exists), last_fetched_at = today),
    by = "tournament_id"
  )
}

#' Advance the historical low-to-high backfill walk by one bounded batch
#'
#' Skips any id already checkpointed (found or confirmed absent), matching
#' run_abr_backfill()'s "never refetch a settled id" discipline. Stops
#' advancing (and records backfill_complete) once the walk has passed
#' crawl_state$max_known_id with nothing left to attempt; a later run that
#' discovers a higher max_known_id resets that flag.
#' @keywords internal
advance_cobra_backfill <- function(lineage, checkpoint, crawl_state, pool_dir) {
  upper_bound <- crawl_state$max_known_id
  if (crawl_state$backfill_next_id > upper_bound) {
    crawl_state$backfill_complete <- TRUE
    return(list(checkpoint = checkpoint, crawl_state = crawl_state))
  }

  batch_end <- min(upper_bound, crawl_state$backfill_next_id + COBRA_BACKFILL_BATCH_SIZE - 1L)
  batch_ids <- seq.int(crawl_state$backfill_next_id, batch_end)
  settled_ids <- checkpoint$tournament_id
  batch_ids <- batch_ids[!as.character(batch_ids) %in% settled_ids]

  for (id in batch_ids) {
    bundle <- scrape_cobra_bundle(lineage, id)
    checkpoint <- record_cobra_bundle(pool_dir, checkpoint, bundle)
  }

  crawl_state$backfill_next_id <- batch_end + 1L
  crawl_state$backfill_complete <- crawl_state$backfill_next_id > upper_bound

  list(checkpoint = checkpoint, crawl_state = crawl_state)
}

#' @keywords internal
write_cobra_object <- function(pool_dir, tournament_id, bundle) {
  object_path <- file.path(pool_dir, sprintf("%s.json", tournament_id))
  jsonlite::write_json(
    list(
      tournament_id = bundle$tournament_id,
      pairings_data = bundle$pairings_data,
      standings_data = bundle$standings_data,
      id_and_faction_data = bundle$id_and_faction_data,
      cut_conversion_rates = bundle$cut_conversion_rates
    ),
    object_path, auto_unbox = TRUE, null = "null"
  )
  Sys.chmod(object_path, mode = "0600")
}

#' Read one tournament's bundle back off the pool
#' @keywords internal
read_cobra_object <- function(pool_dir, tournament_id) {
  object_path <- file.path(pool_dir, sprintf("%s.json", tournament_id))
  # simplifyVector = FALSE to match cobra_get()'s parsing above -- the
  # bundle read back off the pool must have the same nested-list shape
  # the bundle written to it did.
  jsonlite::fromJSON(object_path, simplifyVector = FALSE)
}

#' Digest the whole pool's current content: sorted resolved ids paired
#' with each object file's own content hash, so a refreshed-but-unchanged
#' tournament does not change content_identity and a real edit does.
#' @keywords internal
cobra_content_identity <- function(pool_dir, resolved_ids) {
  ids <- sort(resolved_ids)
  file_hashes <- vapply(ids, function(id) {
    digest::digest(file = file.path(pool_dir, sprintf("%s.json", id)), algo = "sha256")
  }, character(1))
  digest::digest(list(ids, file_hashes), algo = "sha256")
}

#' @keywords internal
check_cobra_pool_nonempty <- function(bundles) {
  ok <- length(bundles) > 0
  list(
    check = "cobra_pool_nonempty",
    status = if (ok) "pass" else "warn",
    message = sprintf("%d tournament bundle(s) in pool", length(bundles))
  )
}

#' Per-attempt shape check: every resolved bundle carries a pairings_data
#' envelope with a $tournament field, matching compare_shape()'s role in
#' R/fetch-nrdb.R -- the one real, honestly-derived signal available for
#' a source with no cross-run field to compare against.
#' @keywords internal
check_cobra_bundle_shape <- function(bundles) {
  bad <- Filter(function(b) is.null(b$pairings_data) || is.null(b$pairings_data$tournament), bundles)
  ok <- length(bad) == 0
  list(
    check = "cobra_bundle_shape",
    status = if (ok) "pass" else "fail",
    message = if (ok) "ok" else sprintf("%d bundle(s) missing pairings_data$tournament", length(bad))
  )
}
