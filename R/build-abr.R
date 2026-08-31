#' Build an api-poll lineage's processed release
#'
#' Dispatches to the abr or nrdb build routine by lineage name, since both
#' lineages share the netrunneR_api_poll class and fetch method but have
#' distinct schemas, allowlists and reconciliation logic.
#'
#' @param lineage A lineage object of class netrunneR_api_poll.
#' @param staged_raw The value returned by the matching internal fetch helper.
#' @param ... Ignored.
#'
#' @export
build_lineage.netrunneR_api_poll <- function(lineage, staged_raw, ...) {
  if (identical(lineage$name, "abr")) {
    build_abr(lineage, staged_raw)
  } else if (identical(lineage$name, "nrdb")) {
    build_nrdb(lineage, staged_raw)
  } else {
    rlang::abort(sprintf("No api-poll build method for lineage '%s'", lineage$name), class = "netrunneR_no_build_method")
  }
}

#' Named allowlist of tournament columns permitted into the processed store
#'
#' The single fail-closed dplyr::select() allowlist: any upstream key not
#' named here is dropped rather than passed through, so a new personal-data
#' field added upstream is excluded by default instead of by omission.
#' Layered with check_deny_pattern() below as an independent second check,
#' not a restatement of this allowlist.
#'
#' This allowlist is layer one of two independent, fail-closed checks
#' that both must run before any DBI::dbWriteTable() call: this
#' dplyr::select() allowlist, and the separate check_deny_pattern()
#' regex scan in build_abr() below (R/validate-helpers.R). Neither
#' layer substitutes for the other -- the redundancy is deliberate
#' defense in depth against personal data reaching the processed
#' store. (ref: DL-002)
#'
#' VENUE COORDINATES ARE ADMITTED DELIBERATELY, and this reverses part of
#' the original DL-002 exclusion, so the reasoning belongs here rather
#' than in a commit message.
#'
#' location_lat and location_lng describe WHERE A TOURNAMENT WAS HELD --
#' a shop, a club, a convention hall. They are a property of the event,
#' not of a person: no field here identifies who attended, who organised,
#' or who won by name, and the deny pattern still rejects every field
#' that would. A venue is the same kind of fact as location_country,
#' which was never excluded; the coordinate is only a more precise
#' spelling of it.
#'
#' They are admitted for the tournament map, which plots venue density
#' as a point layer over a country choropleth. That layer cannot be
#' derived from location_country: a country polygon cannot say whether a
#' country's tournaments sit in one city or thirty.
#'
#' WHAT THIS DOES NOT LICENSE. The exclusion of organiser, creator,
#' contact, player and address fields is untouched and must stay
#' untouched -- those identify people, and a venue does not. If a future
#' upstream field ties a coordinate to a person (a home address, a
#' player's location), it is excluded by the deny pattern on the
#' person-field, not admitted by this precedent.
#' @keywords internal
ABR_TOURNAMENT_ALLOWLIST <- c(
  "id", "title", "date", "format", "location_state", "location_country",
  "location_lat", "location_lng",
  # `type` is the EVENT's category, not a person's: 17 values upstream,
  # from "casual tournament kit" through "store championship" to "worlds
  # championship". It says how much weight a result carries, which is the
  # question a reader of a meta chart is actually asking -- a GNK and a
  # continental championship are not the same evidence.
  #
  # Admitted on the same reasoning as the venue coordinates: it describes
  # the event, and no organiser, player or contact field comes with it.
  "type",
  "players_count", "top_count", "winner_runner_identity", "winner_corp_identity"
)

#' @keywords internal
build_abr <- function(lineage, staged_raw) {
  # all_of() (not any_of()) so a shrunk or mistyped ABR_TOURNAMENT_ALLOWLIST
  # errors immediately instead of silently selecting fewer columns than
  # intended -- any_of() tolerates a missing name, which would fail closed
  # on personal data only by accident.
  tournaments <- dplyr::select(staged_raw$tournaments, dplyr::all_of(ABR_TOURNAMENT_ALLOWLIST))
  tournaments <- dplyr::distinct(tournaments, .data$id, .keep_all = TRUE)

  # The API returns coordinates as STRINGS, and the schema declares them
  # REAL. SQLite is dynamically typed, so a character value lands in a
  # REAL column without complaint and every later comparison is
  # lexicographic -- "9.5" sorts above "10.5", and a map built on that is
  # wrong in a way no error reports. Coerced once here, where the column
  # enters the store, rather than in each consumer.
  #
  # suppressWarnings() because an unparseable coordinate must become NA
  # rather than emit a warning per row: a venue with no recorded location
  # is ordinary, not exceptional, and the map simply has no point to
  # plot for it.
  for (col in c("location_lat", "location_lng")) {
    tournaments[[col]] <- suppressWarnings(as.numeric(tournaments[[col]]))
  }

  # staged_raw$tournament_count is the raw upstream count and does not
  # reflect ids that run_abr_backfill() marked permanent_unavailable and
  # fetch_abr() therefore already excluded from `tournaments` -- that
  # exclusion is the intended tombstone design (see R/abr-backfill.R,
  # R/fetch-abr.R), not data loss. Subtracting permanent_ids' count keeps
  # this check protective against any other cause of a cardinality
  # mismatch (duplicate ids, a pagination bug, real data loss) while no
  # longer flagging the expected, designed exclusion as a failure.
  permanent_count <- length(staged_raw$permanent_ids %||% character(0))
  expected_count <- staged_raw$tournament_count - permanent_count
  id_count_check <- list(
    check = "tournament_id_cardinality",
    status = if (nrow(tournaments) == expected_count) "pass" else "fail",
    message = sprintf(
      "id set cardinality=%d tournament_count=%d (%d permanently unavailable, expected %d)",
      nrow(tournaments), staged_raw$tournament_count, permanent_count, expected_count
    )
  )

  unknown_keys <- setdiff(names(staged_raw$tournaments), ABR_TOURNAMENT_ALLOWLIST)
  unknown_key_check <- list(
    check = "unknown_upstream_keys",
    status = if (length(unknown_keys) == 0) "pass" else "warn",
    message = if (length(unknown_keys) == 0) "ok" else sprintf("Unrecognized upstream keys dropped: %s", paste(unknown_keys, collapse = ", "))
  )

  deny_check <- check_deny_pattern(tournaments, ABR_DENY_PATTERN)
  # Layer two: an independent regex scan of column names, run after the
  # allowlist select() above rather than instead of it -- see the
  # ABR_TOURNAMENT_ALLOWLIST docstring for why both layers are required.
  if (identical(deny_check$status, "fail")) {
    rlang::abort(deny_check$message, class = "netrunneR_deny_pattern_violation")
  }

  processed_dir <- file.path(dirname(staged_raw$raw_dir), "processed")
  fs::dir_create(processed_dir, mode = "2750")
  db_path <- file.path(processed_dir, "abr.sqlite")

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  apply_schema(con, "abr")
  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "tournament", tournaments, append = TRUE)
  })
  Sys.chmod(db_path, mode = "0640")

  br <- build_revision(lineage, build_module_path = "R/build-abr.R")
  # build_revision is computed identically for every lineage, abr
  # included -- this call is not a narrowed or lineage-specific variant.

  list(
    db_path = db_path,
    build_revision = br,
    release_id = sprintf("%s-%s", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), release_entropy_suffix()),
    checks = list(id_count_check, unknown_key_check, deny_check)
  )
}
