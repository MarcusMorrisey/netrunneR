# Tests build_cobra() against a fixture bundle shaped like the real
# pairings_data/standings_data/id_and_faction_data/cut_conversion_rates
# envelopes (per C:\codex\netrunneR\R\sync_nullsignal_tournaments.R's
# already-validated field shapes) -- including the player display-name
# fields (player1_name, player2_name, name_with_pronouns) those envelopes
# actually carry, so this is a regression guard on
# COBRA_*_ALLOWLIST/COBRA_DENY_PATTERN actually excluding them, not just
# a happy-path build test.

cobra_fixture_bundle <- function() {
  list(
    tournament_id = 3L,
    pairings_data = list(
      tournament = list(
        id = 3L, name = "Fixture Cup", slug = "fixture-cup", abr_code = NA,
        private = FALSE, date = "2024-01-01", time_zone = "UTC",
        registration_starts = "2024-01-01T00:00:00Z", tournament_starts = "2024-01-01T12:00:00Z",
        organizer_contact = "leak@example.com", tournament_organizer = "Alex Organizer",
        decklist_required = TRUE, self_registration = FALSE, allow_self_reporting = FALSE,
        swiss_deck_visibility = "always", cut_deck_visibility = "always", swiss_format = "single_sided",
        tournament_type_id = 1L, format_id = 1L, deckbuilding_restriction_id = "standard",
        card_set_id = "standard", created_at = "2023-12-01T00:00:00Z", updated_at = "2024-01-02T00:00:00Z"
      ),
      policy = list(update = FALSE, custom_table_numbering = FALSE),
      stages = list(list(
        id = 10L, name = "Swiss", format = "swiss", is_single_sided = TRUE,
        is_elimination = FALSE, view_decks = TRUE, player_count = 2L,
        rounds = list(list(
          id = 100L, number = 1L, completed = TRUE, pairings_reported = 1L, length_minutes = 45L,
          timer = list(running = FALSE, paused = FALSE, started = TRUE),
          pairings = list(list(
            id = 1000L, table_number = 1L, table_label = "Table 1", reported = TRUE,
            intentional_draw = FALSE, two_for_one = FALSE, score1 = 6, score2 = 3, score_label = "6-3",
            player1 = list(
              id = 501L, name = "Real Name One", name_with_pronouns = "Real Name One (they/them)",
              seed = 1L, side = "corp",
              corp_id = list(name = "Built to Last", faction = "haas-bioroid"),
              runner_id = list(name = "Az McCaffrey", faction = "shaper")
            ),
            player2 = list(
              id = 502L, name = "Real Name Two", name_with_pronouns = "Real Name Two (she/her)",
              seed = 2L, side = "runner",
              corp_id = list(name = "Jinteki Biotech", faction = "jinteki"),
              runner_id = list(name = "Sable", faction = "criminal")
            )
          ))
        ))
      ))
    ),
    standings_data = list(
      tournament_id = 3L,
      stages = list(list(
        format = "swiss", rounds_complete = 1L, any_decks_viewable = TRUE,
        standings = list(list(
          position = 1L,
          player = list(
            id = 501L, active = TRUE, name = "Real Name One", name_with_pronouns = "Real Name One (they/them)",
            corp_id = list(name = "Built to Last", faction = "haas-bioroid"),
            runner_id = list(name = "Az McCaffrey", faction = "shaper")
          ),
          points = 3, sos = 0.5, extended_sos = 0.5, corp_points = 2, runner_points = 1,
          bye_points = 0, seed = 1L, manual_seed = NA, side_bias = 0,
          policy = list(view_decks = TRUE)
        ))
      ))
    ),
    id_and_faction_data = list(
      num_players = 2L,
      corp = list(
        factions = list(`haas-bioroid` = 1L, jinteki = 1L),
        ids = list(`Built to Last` = list(faction = "haas-bioroid", count = 1L))
      ),
      runner = list(factions = list(), ids = list())
    ),
    cut_conversion_rates = list(
      factions = list(corp = list(`haas-bioroid` = list(
        num_swiss_players = 1L, num_cut_players = 1L, cut_conversion_percentage = 100
      ))),
      identities = list()
    )
  )
}

test_that("build_cobra() writes ten tables and excludes every player display-name field", {
  raw_dir <- file.path(withr::local_tempdir(), "raw")
  fs::dir_create(raw_dir)

  li <- new_lineage("cobra", "api_poll", withr::local_tempdir(), base_url = "https://example.test",
                    schema_version = 1L, build_module_path = "R/build-cobra.R")

  bundle <- cobra_fixture_bundle()
  staged_raw <- list(
    raw_dir = raw_dir,
    bundles = list(`3` = bundle),
    recent_index = tibble::tibble(tournament_id = 3L, tournament_type_id = 1L, discovered_at = "2024-01-01T00:00:00Z"),
    checks = list()
  )

  built <- build_cobra(li, staged_raw)
  expect_true(fs::file_exists(built$db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  on.exit(DBI::dbDisconnect(con))

  tournament <- DBI::dbReadTable(con, "tournament")
  expect_identical(tournament$name, "Fixture Cup")
  expect_false(any(grepl("organizer|contact", names(tournament), ignore.case = TRUE)))

  pairing <- DBI::dbReadTable(con, "pairing")
  expect_identical(nrow(pairing), 1L)
  expect_identical(pairing$player1_corp_identity, "Built to Last")
  # stage_name is a legitimate, non-personal allowlisted column (the
  # tournament stage's own name, e.g. "Swiss") -- only the player
  # display-name shapes COBRA_DENY_PATTERN targets must be absent.
  expect_false(any(grepl("player[0-9]*_name|name_with_pronouns", names(pairing), ignore.case = TRUE)))

  standing <- DBI::dbReadTable(con, "standing")
  expect_identical(nrow(standing), 1L)
  expect_false(any(grepl("player[0-9]*_name|name_with_pronouns", names(standing), ignore.case = TRUE)))

  faction_count <- DBI::dbReadTable(con, "faction_count")
  expect_true(nrow(faction_count) >= 1L)

  cut_conversion_faction <- DBI::dbReadTable(con, "cut_conversion_faction")
  expect_identical(nrow(cut_conversion_faction), 1L)

  for (tbl in c("stage", "round", "identity_count", "cut_conversion_identity", "recent_index")) {
    expect_true(tbl %in% DBI::dbListTables(con))
  }
})

test_that("build_lineage.netrunneR_api_poll() dispatches to build_cobra() by lineage name", {
  raw_dir <- file.path(withr::local_tempdir(), "raw")
  fs::dir_create(raw_dir)

  li <- new_lineage("cobra", "api_poll", withr::local_tempdir(), base_url = "https://example.test",
                    schema_version = 1L, build_module_path = "R/build-cobra.R")

  staged_raw <- list(
    raw_dir = raw_dir,
    bundles = list(`3` = cobra_fixture_bundle()),
    recent_index = tibble::tibble(tournament_id = 3L, tournament_type_id = 1L, discovered_at = "2024-01-01T00:00:00Z"),
    checks = list()
  )

  built <- build_lineage(li, staged_raw)
  expect_true(fs::file_exists(built$db_path))
})

test_that("build_cobra() tolerates a bundle with no standings/id-and-faction/cut-conversion data", {
  raw_dir <- file.path(withr::local_tempdir(), "raw")
  fs::dir_create(raw_dir)

  li <- new_lineage("cobra", "api_poll", withr::local_tempdir(), base_url = "https://example.test",
                    schema_version = 1L, build_module_path = "R/build-cobra.R")

  bundle <- cobra_fixture_bundle()
  bundle$standings_data <- NULL
  bundle$id_and_faction_data <- NULL
  bundle$cut_conversion_rates <- NULL

  staged_raw <- list(
    raw_dir = raw_dir,
    bundles = list(`3` = bundle),
    recent_index = tibble::tibble(tournament_id = 3L, tournament_type_id = 1L, discovered_at = "2024-01-01T00:00:00Z"),
    checks = list()
  )

  built <- build_cobra(li, staged_raw)
  expect_true(fs::file_exists(built$db_path))
})
