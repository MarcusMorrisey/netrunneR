# A real, promoted release store in a temporary directory.
#
# This is what test-integration-app.R was waiting on. Nothing here
# hand-builds the store layout: it stages a release exactly as run_sync()
# does (under store_root/staging/<id>, outside releases/, since promote()
# MOVES the staging directory) and then calls the package's own promote(),
# so the fixture exercises the same promote/swap_active/resolve_release
# path production uses rather than a parallel imitation of it that could
# drift.
#
# Reaching lineage()'s store_root at all requires the NETRUNNER_STORE_BASE
# seam (store_base(), R/lineage.R) -- resolve_active_release() calls
# lineage(), which had /data hardcoded.

#' Table contents each lineage's processed database is seeded with
#'
#' Reuses helper-mini-pool.R rather than inventing a second sample, so
#' the app-level tests and the matchup unit tests describe the same
#' fixture world.
#' @keywords internal
STORE_FIXTURE_TABLES <- function() {
  list(
    cardpool = list(
      db = "cardpool.sqlite",
      tables = list(card = mini_pool_cardpool())
    ),
    implementation = list(
      db = "implementation.sqlite",
      tables = list(ice_breaker_traits = mini_pool_ice_breaker_traits())
    ),
    abr = list(
      db = "abr.sqlite",
      tables = list(tournament = data.frame(
        id = "9001", title = "Fixture Store Championship", date = "2026.01.10.",
        format = "standard", type = "store championship",
        location_state = "CA", location_country = "United States",
        location_lat = 37.77, location_lng = -122.42,
        players_count = 16L, top_count = 4L,
        winner_runner_identity = "21063", winner_corp_identity = "21054",
        stringsAsFactors = FALSE
      ))
    ),
    # A cobra release WITH tournament_merged (DL-049/M-001) -- the normal,
    # post-merge shape. See test-store-fixture.R for a release predating
    # the table, staged inline there rather than as a second named entry
    # here, since it is a deliberately incomplete/older shape rather than
    # a second lineage this fixture registry otherwise models one-per-name.
    cobra = list(
      db = "cobra.sqlite",
      tables = list(
        tournament_merged = data.frame(
          id = "abr:9001", title = "Fixture Store Championship", date = "2026.01.10.",
          format = "standard", type = "store championship",
          location_state = "CA", location_country = "United States",
          location_lat = 37.77, location_lng = -122.42,
          players_count = 16L, top_count = 4L,
          winner_runner_identity = "21063", winner_corp_identity = "21054",
          stringsAsFactors = FALSE
        ),
        tournament_merged_source = data.frame(
          merged_id = "abr:9001", source = "abr", source_id = "9001", match_tier = "single",
          stringsAsFactors = FALSE
        )
      )
    )
  )
}

#' Promote a fixture release for each named lineage into a temp store
#'
#' @param lineages Character vector of lineage names to promote. Pass a
#'   subset (or `character(0)`) to exercise the missing-release paths --
#'   a lineage left out has a store directory but no `active` symlink,
#'   which is exactly the state resolve_release() aborts on.
#' @param .env Environment the temp directory and the environment
#'   variable are scoped to; defaults to the calling test.
#' @return The store base path, invisibly.
local_store_fixture <- function(lineages = c("cardpool", "implementation"),
                                .env = parent.frame()) {
  base <- withr::local_tempdir(.local_envir = .env)
  withr::local_envvar(c(NETRUNNER_STORE_BASE = base), .local_envir = .env)

  spec <- STORE_FIXTURE_TABLES()
  for (name in lineages) {
    entry <- spec[[name]]
    if (is.null(entry)) {
      rlang::abort(sprintf("No store fixture defined for lineage '%s'", name))
    }
    store_root <- file.path(base, name)
    staging_dir <- file.path(store_root, "staging", "fixture")
    processed_dir <- file.path(staging_dir, "processed")
    fs::dir_create(processed_dir)

    con <- DBI::dbConnect(RSQLite::SQLite(), file.path(processed_dir, entry$db))
    for (table_name in names(entry$tables)) {
      DBI::dbWriteTable(con, table_name, entry$tables[[table_name]])
    }
    DBI::dbDisconnect(con)

    promote(store_root, staging_dir, "fixture-release")
  }

  invisible(base)
}
