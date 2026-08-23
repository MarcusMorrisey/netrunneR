#' Fail-closed dplyr::select() allowlists for the review and ruling
#' tables, matching inst/sql/schema/nrdb.sql exactly. The real
#' NetrunnerDB /reviews and /rulings envelopes (confirmed live against
#' netrunnerdb.com/api/2.0/public) do not match the schema's original
#' invented column names (card_code, text, created_at never existed
#' upstream -- same root mistake the decklist table had). /rulings
#' carries no id field at all, so the ruling table has no primary key;
#' /reviews and /rulings both use `title` for the card's title (there is
#' no card_code), and /reviews carries a `user` field with the
#' reviewer's public NetrunnerDB username -- kept here on the same
#' precedent as ABR_TOURNAMENT_ALLOWLIST's winner_runner_identity/
#' winner_corp_identity: a publicly displayed identity attached to
#' public content, not private personal data.
#' @keywords internal
NRDB_REVIEW_ALLOWLIST <- c("id", "title", "user", "ruling", "votes", "comments", "date_create", "date_update")

#' Same allowlist, for the ruling table.
#' @keywords internal
NRDB_RULING_ALLOWLIST <- c("title", "ruling", "date_update", "nsg_rules_team_verified")

#' Build the nrdb release
#'
#' Builds the nrdb review and ruling tables from the fetched envelopes.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "nrdb".
#' @param staged_raw The value returned by fetch_nrdb().
#'
#' @keywords internal
build_nrdb <- function(lineage, staged_raw) {
  # all_of() (not any_of()) so a shrunk or mistyped allowlist errors
  # immediately instead of silently selecting fewer columns than
  # intended, matching ABR_TOURNAMENT_ALLOWLIST's discipline.
  reviews <- dplyr::select(tibble::as_tibble(staged_raw$reviews$data), dplyr::all_of(NRDB_REVIEW_ALLOWLIST))
  rulings <- dplyr::select(tibble::as_tibble(staged_raw$rulings$data), dplyr::all_of(NRDB_RULING_ALLOWLIST))

  db_path <- file.path(dirname(staged_raw$raw_dir), "processed", "nrdb.sqlite")
  fs::dir_create(dirname(db_path))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  apply_schema(con, "nrdb")
  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "review", reviews, append = TRUE)
    DBI::dbWriteTable(con, "ruling", rulings, append = TRUE)
  })

  br <- build_revision(lineage, build_module_path = "R/build-nrdb.R")
  # build_revision is computed identically for every lineage, nrdb
  # included -- this call is not a narrowed or lineage-specific variant.

  list(
    db_path = db_path,
    build_revision = br,
    release_id = sprintf("%s-%s", format(Sys.time(), "%Y%m%dT%H%M%SZ", tz = "UTC"), release_entropy_suffix()),
    checks = staged_raw$checks
  )
}
