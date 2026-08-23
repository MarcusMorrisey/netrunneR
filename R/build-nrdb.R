#' Build the nrdb release
#'
#' Builds the nrdb review and ruling tables from the fetched envelopes.
#'
#' @param lineage A lineage object of class netrunneR_api_poll named "nrdb".
#' @param staged_raw The value returned by fetch_nrdb().
#'
#' @keywords internal
build_nrdb <- function(lineage, staged_raw) {
  db_path <- file.path(dirname(staged_raw$raw_dir), "processed", "nrdb.sqlite")
  fs::dir_create(dirname(db_path))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  apply_schema(con, "nrdb")
  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "review", tibble::as_tibble(staged_raw$reviews$data), append = TRUE)
    DBI::dbWriteTable(con, "ruling", tibble::as_tibble(staged_raw$rulings$data), append = TRUE)
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
