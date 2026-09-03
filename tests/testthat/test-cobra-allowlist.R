# Exercises both fail-closed layers from R/build-cobra.R and
# R/validate-helpers.R across all ten Cobra tables: the per-table
# dplyr::select(all_of(...)) allowlists and the independent
# check_deny_pattern(COBRA_DENY_PATTERN) regex scan, plus the
# flatten_cobra_*() helpers that turn one pool bundle's nested,
# simplifyVector = FALSE shape into rows.

# A single, reasonably complete bundle in the same nested-list shape
# cobra_get(..., as = "json") returns (simplifyVector = FALSE): scalars
# are length-1 lists, not atomic vectors, matching what
# flatten_cobra_*() actually walks with $ access.
cobra_fixture_bundle <- function() {
  list(
    tournament_id = 42L,
    pairings_data = list(
      tournament = list(
        id = 42L, name = "Fixture Cup", slug = "fixture-cup", abr_code = "fc1",
        private = FALSE, date = "2026-01-10", time_zone = "UTC",
        registration_starts = "2026-01-09T00:00:00Z", tournament_starts = "2026-01-10T09:00:00Z",
        decklist_required = TRUE, self_registration = TRUE, allow_self_reporting = FALSE,
        swiss_deck_visibility = "hidden", cut_deck_visibility = "hidden", swiss_format = "standard",
        tournament_type_id = 3L, format_id = 1L, deckbuilding_restriction_id = "standard",
        card_set_id = "core", created_at = "2026-01-01T00:00:00Z", updated_at = "2026-01-02T00:00:00Z"
      ),
      policy = list(update = TRUE, custom_table_numbering = FALSE),
      stages = list(list(
        id = 1L, name = "Swiss", format = "standard", is_single_sided = FALSE,
        is_elimination = FALSE, view_decks = TRUE, player_count = 2L,
        rounds = list(list(
          id = 10L, number = 1L, completed = TRUE, pairings_reported = 1L, length_minutes = 65L,
          timer = list(running = FALSE, paused = FALSE, started = TRUE),
          pairings = list(list(
            id = 100L, table_number = 1L, table_label = "Table 1", reported = TRUE,
            intentional_draw = FALSE, two_for_one = FALSE, score1 = 6, score2 = 0, score_label = "6-0",
            player1 = list(
              id = 1001L, seed = 1L, side = "corp", name = "Alice Example",
              corp_id = list(name = "Jinteki: Personal Evolution", faction = "jinteki"),
              runner_id = list(name = NULL, faction = NULL)
            ),
            player2 = list(
              id = 1002L, seed = 2L, side = "runner", name = "Bob Example",
              corp_id = list(name = NULL, faction = NULL),
              runner_id = list(name = "Kate \"Mac\" McCaffrey: Digital Tinker", faction = "shaper")
            )
          ))
        ))
      ))
    ),
    standings_data = list(
      tournament_id = 42L,
      stages = list(list(
        format = "standard", rounds_complete = 1L, any_decks_viewable = TRUE,
        standings = list(list(
          position = 1L,
          player = list(
            id = 1001L, active = TRUE, name = "Alice Example",
            corp_id = list(name = "Jinteki: Personal Evolution", faction = "jinteki"),
            runner_id = list(name = NULL, faction = NULL)
          ),
          points = 6, sos = 0.5, extended_sos = 0.5, corp_points = 6, runner_points = 0,
          bye_points = 0, seed = 1L, manual_seed = NULL, side_bias = 0,
          policy = list(view_decks = TRUE)
        ))
      ))
    ),
    id_and_faction_data = list(
      num_players = 2L,
      corp = list(
        factions = list(jinteki = 1L),
        ids = list(`Jinteki: Personal Evolution` = list(faction = "jinteki", count = 1L))
      ),
      runner = list(
        factions = list(shaper = 1L),
        ids = list(`Kate "Mac" McCaffrey: Digital Tinker` = list(faction = "shaper", count = 1L))
      )
    ),
    cut_conversion_rates = list(
      factions = list(corp = list(jinteki = list(
        num_swiss_players = 1L, num_cut_players = 1L, cut_conversion_percentage = 100
      ))),
      identities = list(corp = list(`Jinteki: Personal Evolution` = list(
        faction = "jinteki", num_swiss_players = 1L, num_cut_players = 1L, cut_conversion_percentage = 100
      )))
    )
  )
}

cobra_fixture_lineage <- function() {
  new_lineage("cobra", "api_poll", withr::local_tempdir(), schema_version = 1L,
              build_module_path = "R/build-cobra.R")
}

cobra_fixture_staged_raw <- function(bundles, recent_index = NULL) {
  root <- withr::local_tempdir()
  raw_dir <- file.path(root, "raw")
  dir.create(raw_dir)
  list(
    raw_dir = raw_dir,
    bundles = bundles,
    recent_index = recent_index %||% tibble::tibble(
      tournament_id = integer(0), tournament_type_id = integer(0), discovered_at = character(0)
    ),
    checks = list()
  )
}

test_that("build_cobra() flattens one bundle into every table with the allowlisted columns only", {
  bundle <- cobra_fixture_bundle()
  li <- cobra_fixture_lineage()
  staged_raw <- cobra_fixture_staged_raw(list(`42` = bundle))

  built <- build_cobra(li, staged_raw)

  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  withr::defer(DBI::dbDisconnect(con))

  tournaments <- dplyr::collect(dplyr::tbl(con, "tournament"))
  expect_setequal(names(tournaments), COBRA_TOURNAMENT_ALLOWLIST)
  expect_identical(tournaments$name, "Fixture Cup")
  expect_identical(tournaments$stage_count, 1L)
  expect_identical(tournaments$round_count, 1L)
  expect_identical(tournaments$pairing_count, 1L)

  stages <- dplyr::collect(dplyr::tbl(con, "stage"))
  expect_setequal(names(stages), COBRA_STAGE_ALLOWLIST)
  expect_identical(nrow(stages), 1L)

  rounds <- dplyr::collect(dplyr::tbl(con, "round"))
  expect_setequal(names(rounds), COBRA_ROUND_ALLOWLIST)
  expect_identical(nrow(rounds), 1L)

  pairings <- dplyr::collect(dplyr::tbl(con, "pairing"))
  expect_setequal(names(pairings), COBRA_PAIRING_ALLOWLIST)
  expect_identical(pairings$player1_corp_identity, "Jinteki: Personal Evolution")
  expect_identical(pairings$player2_runner_identity, "Kate \"Mac\" McCaffrey: Digital Tinker")

  standings <- dplyr::collect(dplyr::tbl(con, "standing"))
  expect_setequal(names(standings), COBRA_STANDING_ALLOWLIST)
  expect_identical(standings$corp_identity, "Jinteki: Personal Evolution")

  faction_counts <- dplyr::collect(dplyr::tbl(con, "faction_count"))
  expect_setequal(names(faction_counts), COBRA_FACTION_COUNT_ALLOWLIST)
  expect_true(any(faction_counts$faction == "jinteki"))

  identity_counts <- dplyr::collect(dplyr::tbl(con, "identity_count"))
  expect_setequal(names(identity_counts), COBRA_IDENTITY_COUNT_ALLOWLIST)
  expect_true(any(identity_counts$identity_name == "Jinteki: Personal Evolution"))

  cut_factions <- dplyr::collect(dplyr::tbl(con, "cut_conversion_faction"))
  expect_setequal(names(cut_factions), COBRA_CUT_CONVERSION_FACTION_ALLOWLIST)

  cut_identities <- dplyr::collect(dplyr::tbl(con, "cut_conversion_identity"))
  expect_setequal(names(cut_identities), COBRA_CUT_CONVERSION_IDENTITY_ALLOWLIST)
})

# The whole point of this lineage's personal-data defense: pairings and
# standings carry real player display names (player1$name, player2$name,
# player$name) directly in the upstream response, unlike ABR's
# identity-only fields. Neither flatten_cobra_pairings() nor
# flatten_cobra_standings() reads bundle$..$player*$name at all, and the
# allowlist select() would drop it even if a future edit started reading
# it -- pin both.
test_that("build_cobra() never writes a player display name column, even though the upstream fixture carries one", {
  bundle <- cobra_fixture_bundle()
  li <- cobra_fixture_lineage()
  staged_raw <- cobra_fixture_staged_raw(list(`42` = bundle))

  built <- build_cobra(li, staged_raw)

  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  withr::defer(DBI::dbDisconnect(con))

  # "name" alone is too broad a substring here -- COBRA_PAIRING_ALLOWLIST
  # legitimately admits stage_name (the event's stage, e.g. "Swiss"), not
  # a person's anything. What must never appear is a *player* name field.
  for (tbl in c("pairing", "standing")) {
    cols <- DBI::dbListFields(con, tbl)
    expect_false(any(grepl("player.*name|^name$", cols)), info = tbl)
  }
})

test_that("dplyr::all_of() errors closed when a Cobra allowlist names a missing column", {
  df <- tibble::tibble(tournament_id = "t1")
  expect_error(dplyr::select(df, dplyr::all_of(COBRA_TOURNAMENT_ALLOWLIST)))
})

test_that("check_deny_pattern() with COBRA_DENY_PATTERN flags every personal-data field shape Cobra carries", {
  for (col in c("player1_name", "player2_name", "name_with_pronouns", "player_handle",
                "organizer", "contact", "email", "address")) {
    df <- tibble::tibble(tournament_id = "t1")
    df[[col]] <- "x"
    expect_identical(check_deny_pattern(df, COBRA_DENY_PATTERN)$status, "fail", info = col)
  }
})

test_that("COBRA_DENY_PATTERN still rejects coordinates, unlike the ABR-widened ABR_DENY_PATTERN", {
  # Cobra carries no venue-coordinate field at all -- there is no
  # equivalent of the ABR_TOURNAMENT_ALLOWLIST reversal to make here.
  df <- tibble::tibble(tournament_id = "t1", latitude = 51.5)
  expect_identical(check_deny_pattern(df, COBRA_DENY_PATTERN)$status, "fail")
})

test_that("cobra_bind_allowlisted() returns a well-shaped zero-row tibble when every bundle contributes nothing", {
  li <- cobra_fixture_lineage()
  bare_bundle <- list(tournament_id = 1L, pairings_data = list(tournament = list(id = 1L), stages = list()),
                      standings_data = NULL, id_and_faction_data = NULL, cut_conversion_rates = NULL)
  staged_raw <- cobra_fixture_staged_raw(list(`1` = bare_bundle))

  built <- build_cobra(li, staged_raw)

  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  withr::defer(DBI::dbDisconnect(con))

  expect_identical(nrow(dplyr::collect(dplyr::tbl(con, "tournament"))), 1L)
  expect_identical(nrow(dplyr::collect(dplyr::tbl(con, "stage"))), 0L)
  expect_identical(nrow(dplyr::collect(dplyr::tbl(con, "standing"))), 0L)
  expect_identical(nrow(dplyr::collect(dplyr::tbl(con, "faction_count"))), 0L)
  expect_identical(nrow(dplyr::collect(dplyr::tbl(con, "cut_conversion_faction"))), 0L)
})

test_that("build_cobra() writes the recent_index provenance table", {
  li <- cobra_fixture_lineage()
  recent_index <- tibble::tibble(
    tournament_id = 42L, tournament_type_id = 3L, discovered_at = "2026-01-01T00:00:00Z"
  )
  staged_raw <- cobra_fixture_staged_raw(list(`42` = cobra_fixture_bundle()), recent_index = recent_index)

  built <- build_cobra(li, staged_raw)

  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  withr::defer(DBI::dbDisconnect(con))
  got <- dplyr::collect(dplyr::tbl(con, "recent_index"))
  expect_setequal(names(got), COBRA_RECENT_INDEX_ALLOWLIST)
  expect_identical(got$tournament_type_id, 3L)
})
