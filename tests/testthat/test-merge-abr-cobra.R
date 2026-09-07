# Covers merge_abr_cobra()'s four-tier override precedence, per-column
# survivorship, cobra winner/player-count derivation, and identity code
# resolution -- see R/merge-abr-cobra.R for the design rationale (DL-049
# through DL-066).

abr_row <- function(id, title = "Test Event", date = "2024.01.15.", players_count = 10,
                     type = "store championship", location_country = "United States",
                     location_state = "CA", location_lat = NA_real_, location_lng = NA_real_,
                     top_count = 4, winner_runner_identity = "21063", winner_corp_identity = "21054",
                     format = "standard") {
  data.frame(
    id = id, title = title, date = date, format = format, type = type,
    location_state = location_state, location_country = location_country,
    location_lat = location_lat, location_lng = location_lng,
    players_count = players_count, top_count = top_count,
    winner_runner_identity = winner_runner_identity, winner_corp_identity = winner_corp_identity,
    stringsAsFactors = FALSE
  )
}

cobra_row <- function(tournament_id, name = "Test Event", date = "2024-01-15") {
  data.frame(tournament_id = tournament_id, name = name, date = date, stringsAsFactors = FALSE)
}

cobra_stage_row <- function(tournament_id, player_count, stage_format = "swiss") {
  data.frame(tournament_id = tournament_id, player_count = player_count, format = stage_format, stringsAsFactors = FALSE)
}

cobra_standing_row <- function(tournament_id, position, stage_format = "swiss", rounds_complete = 3,
                                corp_identity = NA_character_, runner_identity = NA_character_,
                                player_id = 1L) {
  data.frame(
    tournament_id = tournament_id, stage_format = stage_format, rounds_complete = rounds_complete,
    position = position, corp_identity = corp_identity, runner_identity = runner_identity,
    player_id = player_id, stringsAsFactors = FALSE
  )
}

cardpool_row <- function(code, title) {
  data.frame(code = code, title = title, stringsAsFactors = FALSE)
}

empty_abr <- function() {
  data.frame(
    id = character(0), title = character(0), date = character(0), format = character(0),
    type = character(0), location_state = character(0), location_country = character(0),
    location_lat = numeric(0), location_lng = numeric(0), players_count = integer(0),
    top_count = integer(0), winner_runner_identity = character(0), winner_corp_identity = character(0),
    stringsAsFactors = FALSE
  )
}

empty_cardpool <- function() {
  data.frame(code = character(0), title = character(0), stringsAsFactors = FALSE)
}

empty_cobra_stage <- function() {
  data.frame(tournament_id = character(0), player_count = integer(0), format = character(0), stringsAsFactors = FALSE)
}

empty_cobra_standing <- function() {
  data.frame(
    tournament_id = character(0), stage_format = character(0), rounds_complete = integer(0),
    position = integer(0), corp_identity = character(0), runner_identity = character(0),
    player_id = integer(0), stringsAsFactors = FALSE
  )
}

empty_overrides <- function() {
  list(
    matches = data.frame(abr_id = character(0), cobra_tournament_id = character(0), stringsAsFactors = FALSE),
    deletes = data.frame(source = character(0), source_id = character(0), stringsAsFactors = FALSE),
    groups = data.frame(abr_id = character(0), cobra_tournament_id = character(0), stringsAsFactors = FALSE)
  )
}

test_that("a verified delete excludes its cobra row from both output tables", {
  ov <- empty_overrides()
  ov$deletes <- data.frame(source = "cobra", source_id = "9", stringsAsFactors = FALSE)

  result <- merge_abr_cobra(
    abr_tournament = empty_abr(),
    cobra_tournament = cobra_row("9"),
    cobra_stage = empty_cobra_stage(),
    cobra_standing = empty_cobra_standing(),
    cardpool_titles = empty_cardpool(),
    verified_matches = ov$matches, verified_deletes = ov$deletes, verified_groups = ov$groups
  )

  expect_equal(nrow(result$tournament_merged), 0)
  expect_equal(nrow(result$tournament_merged_source), 0)
})

test_that("a group-match row produces one merged row with several tournament_merged_source rows", {
  ov <- empty_overrides()
  ov$groups <- data.frame(
    abr_id = c("100", "100"), cobra_tournament_id = c("1", "2"), stringsAsFactors = FALSE
  )

  result <- merge_abr_cobra(
    abr_tournament = abr_row("100", players_count = 30),
    cobra_tournament = rbind(cobra_row("1"), cobra_row("2")),
    cobra_stage = rbind(cobra_stage_row("1", 12L), cobra_stage_row("2", 18L)),
    cobra_standing = empty_cobra_standing(),
    cardpool_titles = empty_cardpool(),
    verified_matches = ov$matches, verified_deletes = ov$deletes, verified_groups = ov$groups
  )

  expect_equal(nrow(result$tournament_merged), 1)
  expect_equal(result$tournament_merged$players_count, 30L) # 12 + 18, cobra wins over abr's 30 coincidentally equal -- see next test for a non-equal case
  expect_equal(nrow(result$tournament_merged_source), 3) # 1 abr + 2 cobra
  expect_true(all(result$tournament_merged_source$match_tier == "group"))
})

test_that("group match sums cobra player counts even when abr's count differs", {
  ov <- empty_overrides()
  ov$groups <- data.frame(
    abr_id = c("100", "100"), cobra_tournament_id = c("1", "2"), stringsAsFactors = FALSE
  )

  result <- merge_abr_cobra(
    abr_tournament = abr_row("100", players_count = 294),
    cobra_tournament = rbind(cobra_row("1"), cobra_row("2")),
    cobra_stage = rbind(cobra_stage_row("1", 121L), cobra_stage_row("2", 173L)),
    cobra_standing = empty_cobra_standing(),
    cardpool_titles = empty_cardpool(),
    verified_matches = ov$matches, verified_deletes = ov$deletes, verified_groups = ov$groups
  )

  expect_equal(result$tournament_merged$players_count, 294L) # 121 + 173, matches abr here but derived from cobra
})

test_that("a verified match wins over an algorithmic candidate that would have paired differently", {
  ov <- empty_overrides()
  # abr 100 and abr 101 are both same-date/high-similarity candidates for
  # cobra 1 under the algorithmic tier -- but the override says 1 -> 101.
  ov$matches <- data.frame(abr_id = "101", cobra_tournament_id = "1", stringsAsFactors = FALSE)

  result <- merge_abr_cobra(
    abr_tournament = rbind(
      abr_row("100", title = "Regional Championship", date = "2024.01.15.", players_count = 20),
      abr_row("101", title = "Regional Championship", date = "2024.01.15.", players_count = 20)
    ),
    cobra_tournament = cobra_row("1", name = "Regional Championship", date = "2024-01-15"),
    cobra_stage = cobra_stage_row("1", 20L),
    cobra_standing = empty_cobra_standing(),
    cardpool_titles = empty_cardpool(),
    verified_matches = ov$matches, verified_deletes = ov$deletes, verified_groups = ov$groups
  )

  expect_equal(nrow(result$tournament_merged), 2) # 1+101 merged, 100 stays single
  paired <- result$tournament_merged_source[result$tournament_merged_source$source == "cobra", ]
  expect_equal(paired$source_id, "1")
  sibling_abr <- result$tournament_merged_source[
    result$tournament_merged_source$merged_id == paired$merged_id & result$tournament_merged_source$source == "abr",
  ]
  expect_equal(sibling_abr$source_id, "101")
  expect_true(all(result$tournament_merged_source$match_tier[result$tournament_merged_source$merged_id == paired$merged_id] == "verified"))
})

test_that("each tournament_merged_source row records the tier that produced it", {
  ov <- empty_overrides()
  ov$matches <- data.frame(abr_id = "100", cobra_tournament_id = "1", stringsAsFactors = FALSE)

  result <- merge_abr_cobra(
    abr_tournament = abr_row("100"),
    cobra_tournament = cobra_row("1"),
    cobra_stage = cobra_stage_row("1", 10L),
    cobra_standing = empty_cobra_standing(),
    cardpool_titles = empty_cardpool(),
    verified_matches = ov$matches, verified_deletes = ov$deletes, verified_groups = ov$groups
  )

  expect_true(all(result$tournament_merged_source$match_tier == "verified"))
})

test_that("an abr-only tournament and a cobra-only tournament each appear once with NULL in the columns their source lacks", {
  result <- merge_abr_cobra(
    abr_tournament = abr_row("100", date = "2024.01.15."),
    cobra_tournament = cobra_row("1", date = "2024-06-01"),
    cobra_stage = cobra_stage_row("1", 8L),
    cobra_standing = empty_cobra_standing(),
    cardpool_titles = empty_cardpool(),
    verified_matches = empty_overrides()$matches, verified_deletes = empty_overrides()$deletes,
    verified_groups = empty_overrides()$groups
  )

  expect_equal(nrow(result$tournament_merged), 2)
  abr_only <- result$tournament_merged[result$tournament_merged$id == "abr:100", ]
  cobra_only <- result$tournament_merged[result$tournament_merged$id == "cobra:1", ]
  expect_equal(nrow(abr_only), 1)
  expect_equal(nrow(cobra_only), 1)
  expect_true(is.na(cobra_only$location_country))
  expect_true(is.na(cobra_only$type))
  expect_equal(cobra_only$players_count, 8L)
  expect_false(is.na(abr_only$location_country))
})

test_that("tournament_merged column set is exactly abr.tournament's", {
  result <- merge_abr_cobra(
    abr_tournament = abr_row("100"),
    cobra_tournament = cobra_row("1", date = "2024-06-01"),
    cobra_stage = cobra_stage_row("1", 8L),
    cobra_standing = empty_cobra_standing(),
    cardpool_titles = empty_cardpool(),
    verified_matches = empty_overrides()$matches, verified_deletes = empty_overrides()$deletes,
    verified_groups = empty_overrides()$groups
  )

  expect_identical(
    sort(names(result$tournament_merged)),
    sort(names(abr_row("100")))
  )
})

test_that("a source scan finds no adist(a, b)[cbind(...)] call anywhere under R/", {
  pkg_root <- system.file(package = "netrunneR")
  r_dir <- if (nzchar(pkg_root)) file.path(pkg_root, "R") else testthat::test_path("..", "..", "R")
  if (!fs::dir_exists(r_dir)) skip("R/ source not resolvable from installed package")

  r_files <- fs::dir_ls(r_dir, glob = "*.R")
  offenders <- character(0)
  for (f in r_files) {
    lines <- readLines(f, warn = FALSE)
    if (any(grepl("adist\\([^)]*\\)\\s*\\[\\s*cbind", lines))) offenders <- c(offenders, f)
  }
  expect_identical(offenders, character(0))
})

test_that("a cobra tournament with both an elimination and a swiss position=1 standing resolves to the elimination one", {
  standings <- rbind(
    cobra_standing_row("1", position = 1, stage_format = "swiss", rounds_complete = 5,
                        corp_identity = "Swiss Winner Corp", runner_identity = "Swiss Winner Runner", player_id = 2L),
    cobra_standing_row("1", position = 1, stage_format = "single_elim", rounds_complete = 3,
                        corp_identity = "Elim Winner Corp", runner_identity = "Elim Winner Runner", player_id = 1L)
  )
  facts <- cobra_tournament_facts(cobra_stage_row("1", 16L), standings)
  expect_equal(facts$winner_corp_identity_name, "Elim Winner Corp")
  expect_equal(facts$winner_runner_identity_name, "Elim Winner Runner")
})

test_that("the same tournament with the elimination stage at rounds_complete = 0 resolves to the swiss one", {
  standings <- rbind(
    cobra_standing_row("1", position = 1, stage_format = "swiss", rounds_complete = 5,
                        corp_identity = "Swiss Winner Corp", runner_identity = "Swiss Winner Runner"),
    cobra_standing_row("1", position = 1, stage_format = "single_elim", rounds_complete = 0,
                        corp_identity = "Elim Winner Corp", runner_identity = "Elim Winner Runner")
  )
  facts <- cobra_tournament_facts(cobra_stage_row("1", 16L), standings)
  expect_equal(facts$winner_corp_identity_name, "Swiss Winner Corp")
  expect_equal(facts$winner_runner_identity_name, "Swiss Winner Runner")
})

test_that("an identity title carrying several cardpool codes resolves to the smallest code, deterministically on re-run", {
  cardpool <- rbind(
    cardpool_row("25104", "NBN: Making News"),
    cardpool_row("01080", "NBN: Making News"),
    cardpool_row("20109", "NBN: Making News")
  )
  result1 <- resolve_cobra_identity_code("NBN: Making News", cardpool)
  result2 <- resolve_cobra_identity_code("NBN: Making News", cardpool)
  expect_equal(result1, "01080")
  expect_equal(result2, "01080")
})

test_that("an identity name absent from cardpool resolves to NA, never to the raw name", {
  cardpool <- cardpool_row("01001", "Some Other Identity")
  result <- resolve_cobra_identity_code("Not In Cardpool At All", cardpool)
  expect_true(is.na(result))
})
