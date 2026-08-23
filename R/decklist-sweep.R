#' Sweep decklists by date over an inclusive UTC range
#'
#' Fetches decklists by date over an inclusive UTC range recorded in the
#' manifest as sweep_start, sweep_end and sweep_timezone. The initial
#' sweep starts 30 days before the earliest tournament date in the active
#' abr release; an incremental sweep starts 30 days before the last
#' completed sweep_end; an older tournament appearing in a later abr
#' release extends the lower bound. Dates process ascending and a
#' duplicate id resolves later-date-wins with a per-id reconciliation
#' body always winning over a sweep copy.
#'
#' Called from fetch_nrdb() (R/fetch-nrdb.R); every request this
#' function issues goes through nrdb_get(), so no response body it
#' handles bypasses capture_response_body(). (ref: DL-005)
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "nrdb".
#'
#' @export
run_decklist_sweep <- function(lineage) {
  bounds <- decklist_sweep_bounds(lineage)
  dates <- seq(bounds$sweep_start, bounds$sweep_end, by = "day")

  decklists <- purrr::map_dfr(dates, function(d) {
    page <- nrdb_get(lineage$base_url, "/decklists/by-date", list(date = format(d, "%Y-%m-%d")))
    tibble::as_tibble(page$results)
  })
  if (nrow(decklists) == 0) {
    # An entirely empty sweep (no decklists on any date in range) collapses
    # to a zero-column tibble via as_tibble(list()), leaving no `date`/`id`
    # columns for the arrange/distinct calls below.
    decklists <- tibble::tibble(id = character(0), date = character(0))
  }

  decklists <- dplyr::arrange(decklists, .data$date)
  decklists <- dplyr::distinct(decklists, .data$id, .keep_all = TRUE)

  list(
    decklists = decklists,
    sweep_start = bounds$sweep_start,
    sweep_end = bounds$sweep_end,
    sweep_timezone = "UTC"
  )
}

#' Compute the inclusive UTC date range for a decklist sweep
#'
#' The initial sweep spans 30 days before the earliest tournament date in
#' the active abr release through the current UTC date; an incremental
#' sweep spans 30 days before the last sweep_end. A later abr release
#' introducing an older tournament than any seen before extends the lower
#' bound so a decklist tied to that older event is not silently missed.
#' @keywords internal
decklist_sweep_bounds <- function(lineage) {
  today <- as.Date(format(Sys.time(), tz = "UTC"))
  previous <- previous_nrdb_manifest(lineage)

  earliest_abr_date <- earliest_abr_tournament_date()

  if (is.null(previous$sweep_end)) {
    lower <- if (is.null(earliest_abr_date)) today - 30 else earliest_abr_date - 30
    return(list(sweep_start = lower, sweep_end = today))
  }

  last_sweep_end <- as.Date(previous$sweep_end)
  incremental_lower <- last_sweep_end - 30

  lower <- if (!is.null(earliest_abr_date) && (earliest_abr_date - 30) < incremental_lower) {
    earliest_abr_date - 30
  } else {
    incremental_lower
  }

  list(sweep_start = lower, sweep_end = today)
}

#' @keywords internal
earliest_abr_tournament_date <- function() {
  li <- lineage("abr")
  active <- tryCatch(resolve_release(li), error = function(e) NULL)
  if (is.null(active)) return(NULL)
  db_path <- file.path(active$processed_dir, "abr.sqlite")
  if (!fs::file_exists(db_path)) return(NULL)
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  earliest <- DBI::dbGetQuery(con, "SELECT MIN(date) AS earliest FROM tournament")$earliest
  if (length(earliest) == 0 || is.na(earliest)) return(NULL)
  as.Date(earliest)
}
