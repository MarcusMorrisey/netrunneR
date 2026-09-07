# Exercises build_cobra()'s merge step (R/merge-abr-cobra.R) end to end:
# abr-absent degrades to cobra-only merged rows rather than aborting, and
# the build's checks list carries the merged row count and per-tier
# breakdown. See test-cobra-allowlist.R for the ten cobra-only tables'
# own coverage; this file only covers the two merge tables build_cobra()
# writes alongside them.

bc_fixture_bundle <- function(id, name, date, player_count) {
  list(
    tournament_id = id,
    pairings_data = list(
      tournament = list(
        id = id, name = name, slug = NULL, abr_code = NULL, private = FALSE,
        date = date, time_zone = "UTC", registration_starts = NULL, tournament_starts = NULL,
        decklist_required = FALSE, self_registration = FALSE, allow_self_reporting = FALSE,
        swiss_deck_visibility = "hidden", cut_deck_visibility = "hidden", swiss_format = "standard",
        tournament_type_id = 1L, format_id = 1L, deckbuilding_restriction_id = "standard",
        card_set_id = "core", created_at = date, updated_at = date
      ),
      policy = list(update = FALSE, custom_table_numbering = FALSE),
      stages = list(list(
        id = 1L, name = "Swiss", format = "standard", is_single_sided = FALSE,
        is_elimination = FALSE, view_decks = TRUE, player_count = player_count,
        rounds = list()
      ))
    ),
    standings_data = NULL,
    id_and_faction_data = NULL,
    cut_conversion_rates = NULL
  )
}

bc_fixture_lineage <- function() {
  new_lineage("cobra", "api_poll", withr::local_tempdir(), schema_version = 2L,
              build_module_path = "R/build-cobra.R")
}

bc_fixture_staged_raw <- function(bundles) {
  root <- withr::local_tempdir()
  raw_dir <- file.path(root, "raw")
  dir.create(raw_dir)
  list(
    raw_dir = raw_dir,
    bundles = bundles,
    recent_index = tibble::tibble(
      tournament_id = integer(0), tournament_type_id = integer(0), discovered_at = character(0)
    ),
    checks = list()
  )
}

test_that("a build with abr absent writes cobra-only merged rows instead of aborting", {
  # The ambient test environment has no active abr release (no
  # NETRUNNER_STORE_BASE pointed at a real store here), which is exactly
  # the condition build_cobra() must degrade gracefully under rather than
  # abort over -- see query_active_release()'s NULL-on-no-release
  # contract, already relied on elsewhere in this codebase.
  bundle <- bc_fixture_bundle(1L, "Solo Cobra Cup", "2026-01-10", 8L)
  li <- bc_fixture_lineage()
  staged_raw <- bc_fixture_staged_raw(list(`1` = bundle))

  built <- build_cobra(li, staged_raw)

  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  withr::defer(DBI::dbDisconnect(con))

  merged <- dplyr::collect(dplyr::tbl(con, "tournament_merged"))
  sources <- dplyr::collect(dplyr::tbl(con, "tournament_merged_source"))

  expect_equal(nrow(merged), 1)
  expect_identical(merged$id, "cobra:1")
  expect_true(is.na(merged$location_country))

  expect_equal(nrow(sources), 1)
  expect_identical(sources$source, "cobra")
  expect_identical(sources$source_id, "1")
  expect_identical(sources$match_tier, "single")
})

test_that("the checks list carries the merged row count and per-tier breakdown", {
  bundle <- bc_fixture_bundle(2L, "Another Cobra Cup", "2026-02-14", 12L)
  li <- bc_fixture_lineage()
  staged_raw <- bc_fixture_staged_raw(list(`2` = bundle))

  built <- build_cobra(li, staged_raw)

  merge_check <- Filter(function(x) identical(x$check, "tournament_merged_row_count"), built$checks)
  expect_length(merge_check, 1)
  expect_identical(merge_check[[1]]$status, "pass")
  expect_true(grepl("1 merged tournament row", merge_check[[1]]$message))
  expect_true(grepl("single=1", merge_check[[1]]$message))
})
