
fake_identities <- data.frame(
  code = c("id_a", "id_c", "id_s", "id_hb", "id_j", "id_nbn",
           "id_nr", "id_nc"),
  faction_code = c("anarch", "criminal", "shaper",
                   "haas-bioroid", "jinteki", "nbn",
                   "neutral-runner", "neutral-corp"),
  stringsAsFactors = FALSE
)

# Both Neutrals are spelled "Neutral" upstream, exactly as the cardpool
# spells them. That collision is the thing being tested, not a typo.
fake_factions <- data.frame(
  code = c("anarch", "criminal", "shaper", "haas-bioroid", "jinteki", "nbn",
           "neutral-runner", "neutral-corp"),
  name = c("Anarch", "Criminal", "Shaper", "Haas-Bioroid", "Jinteki", "NBN",
           "Neutral", "Neutral"),
  side = c("runner", "runner", "runner", "corp", "corp", "corp",
           "runner", "corp"),
  stringsAsFactors = FALSE
)

fake_tournaments <- data.frame(
  winner_runner_identity = c("id_a", "id_a", "id_c", "id_s", "", NA),
  winner_corp_identity   = c("id_hb", "id_j", "id_j", "id_nbn", "", NA),
  stringsAsFactors = FALSE
)

test_that("tournament_faction_wins counts by side and shares within side", {
  res <- tournament_faction_wins(fake_tournaments, fake_identities, fake_factions)

  runner <- res$wins[res$wins$side == "Runner", ]
  expect_equal(runner$faction_code, c("anarch", "criminal", "shaper"))
  expect_equal(runner$wins, c(2L, 1L, 1L))
  expect_equal(runner$share, c(0.5, 0.25, 0.25))

  corp <- res$wins[res$wins$side == "Corp", ]
  expect_equal(corp$faction_code, c("haas-bioroid", "jinteki", "nbn"))
  expect_equal(corp$wins, c(1L, 2L, 1L))

  # Each side's shares sum to 1 -- they are NOT shares of the combined
  # total, which would be 50/50 by construction.
  expect_equal(sum(runner$share), 1)
  expect_equal(sum(corp$share), 1)
})

test_that("tournaments with no recorded winner are reported, not dropped", {
  res <- tournament_faction_wins(fake_tournaments, fake_identities, fake_factions)
  # Two rows have no winner on either side: one blank, one NA.
  expect_equal(res$undecided, 2L)
  # And they are genuinely out of the counts.
  expect_equal(sum(res$wins$wins[res$wins$side == "Runner"]), 4L)
})

test_that("factions come back in fixed order, not by size", {
  # Shaper wins the most here; it must still be listed after Anarch and
  # Criminal, or the legend reorders itself every time the date filter
  # moves and two periods stop being comparable.
  t <- data.frame(
    winner_runner_identity = c("id_s", "id_s", "id_s", "id_a", "id_c"),
    winner_corp_identity = rep("id_j", 5), stringsAsFactors = FALSE
  )
  res <- tournament_faction_wins(t, fake_identities, fake_factions)
  runner <- res$wins[res$wins$side == "Runner", ]
  expect_equal(runner$faction_code, c("anarch", "criminal", "shaper"))
})

test_that("an unknown faction sorts last rather than vanishing", {
  ids <- rbind(fake_identities,
               data.frame(code = "id_x", faction_code = "brand-new",
                          stringsAsFactors = FALSE))
  t <- data.frame(winner_runner_identity = c("id_x", "id_a"),
                  winner_corp_identity = c("id_j", "id_j"),
                  stringsAsFactors = FALSE)
  res <- tournament_faction_wins(t, ids, fake_factions)
  runner <- res$wins[res$wins$side == "Runner", ]
  expect_equal(runner$faction_code, c("anarch", "brand-new"))
  # No display name for it either, so the raw code shows rather than a
  # blank label that would read as missing data.
  expect_equal(runner$faction[runner$faction_code == "brand-new"], "brand-new")
})

test_that("the ice/breaker pool matches nothing, which is why it is not passed", {
  # Guards the reason `identities` is a separate argument: handed the
  # app's `cards`, every join misses and the chart is empty.
  pool <- data.frame(code = c("01001", "01002"),
                     faction_code = c("anarch", "shaper"),
                     stringsAsFactors = FALSE)
  res <- tournament_faction_wins(fake_tournaments, pool, fake_factions)
  expect_equal(nrow(res$wins), 0L)
})

test_that("empty and missing inputs give an empty frame, not an error", {
  expect_equal(nrow(tournament_faction_wins(NULL, fake_identities)$wins), 0L)
  expect_equal(nrow(tournament_faction_wins(fake_tournaments, NULL)$wins), 0L)
  expect_equal(
    nrow(tournament_faction_wins(fake_tournaments[0, ], fake_identities)$wins), 0L
  )
})

test_that("faction_display_name falls back to the code", {
  expect_equal(faction_display_name(c("nbn", "nope"), fake_factions),
               c("NBN", "nope"))
  expect_equal(faction_display_name("nbn", NULL), "nbn")
})





test_that("every ordered faction has a colour, and vice versa", {
  # FACTION_ORDER is derived from FACTION_COLOURS, so this holds by
  # construction -- it is here to fail if someone splits them.
  expect_setequal(FACTION_ORDER, names(FACTION_COLOURS))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", FACTION_COLOURS)))
})


test_that("a winner on the wrong side is dropped and counted", {
  # Five rows in the live release do this: a Corp identity recorded as the
  # Runner winner, or the reverse. Left in, each puts a faction in a chart
  # it cannot appear in.
  t <- data.frame(
    winner_runner_identity = c("id_a", "id_hb"),   # id_hb is a Corp identity
    winner_corp_identity   = c("id_j", "id_c"),    # id_c is a Runner identity
    stringsAsFactors = FALSE
  )
  res <- tournament_faction_wins(t, fake_identities, fake_factions)

  expect_equal(res$misfiled, 2L)
  expect_equal(res$wins$faction_code[res$wins$side == "Runner"], "anarch")
  expect_equal(res$wins$faction_code[res$wins$side == "Corp"], "jinteki")
  # And the surviving shares are of what is left, not of the raw column.
  expect_equal(res$wins$share, c(1, 1))
})

test_that("without the faction table nothing is filtered, rather than guessed", {
  t <- data.frame(winner_runner_identity = "id_hb",
                  winner_corp_identity = "id_c", stringsAsFactors = FALSE)
  res <- tournament_faction_wins(t, fake_identities, factions = NULL)
  expect_equal(res$misfiled, 0L)
  expect_equal(nrow(res$wins), 2L)
})

test_that("the two Neutrals get distinguishable labels", {
  # The cardpool spells both "Neutral". That is unambiguous in a table
  # with a side column and not in the waffle's single shared legend.
  t <- data.frame(
    winner_runner_identity = c("id_a", "id_nr"),
    winner_corp_identity = c("id_j", "id_nc"), stringsAsFactors = FALSE
  )
  res <- tournament_faction_wins(t, fake_identities, fake_factions)
  expect_setequal(res$wins$faction[grepl("^neutral", res$wins$faction_code)],
                  c("Neutral (Runner)", "Neutral (Corp)"))
  # Nothing else grows a suffix it does not need.
  expect_equal(res$wins$faction[res$wins$faction_code == "anarch"], "Anarch")
})




test_that("an unknown faction survives the side filter", {
  # Dropping it would be filing "not in the lookup yet" under "wrong
  # side", and a genuinely new faction would leave the chart silently.
  # The view names it instead, because it has no colour either.
  ids <- rbind(fake_identities,
               data.frame(code = "id_x", faction_code = "brand-new",
                          stringsAsFactors = FALSE))
  t <- data.frame(winner_runner_identity = "id_x",
                  winner_corp_identity = "id_j", stringsAsFactors = FALSE)
  res <- tournament_faction_wins(t, ids, fake_factions)

  expect_equal(res$misfiled, 0L)
  expect_true("brand-new" %in% res$wins$faction_code)
})


# ---- identity-level counting and the three-level treemap -------------

test_that("identity wins break a faction down to the cards that won", {
  ids <- data.frame(
    code = c("id_a1", "id_a2", "id_c", "id_j"),
    title = c("Hoshiko", "Reina", "Steve", "Palana"),
    faction_code = c("anarch", "anarch", "criminal", "jinteki"),
    stringsAsFactors = FALSE
  )
  t <- data.frame(
    winner_runner_identity = c("id_a1", "id_a1", "id_a2", "id_c"),
    winner_corp_identity = rep("id_j", 4), stringsAsFactors = FALSE
  )
  w <- tournament_identity_wins(t, ids, fake_factions)

  runner <- w[w$side == "Runner", ]
  # Ordered by faction order, then wins descending within a faction.
  expect_equal(runner$identity, c("Hoshiko", "Reina", "Steve"))
  expect_equal(runner$wins, c(2L, 1L, 1L))
  expect_equal(runner$faction_code, c("anarch", "anarch", "criminal"))
})

test_that("identity wins fall back to the code when a release has no titles", {
  # A visible raw code beats a blank label, which reads as missing data.
  ids <- data.frame(code = "id_a1", faction_code = "anarch",
                    stringsAsFactors = FALSE)
  t <- data.frame(winner_runner_identity = "id_a1",
                  winner_corp_identity = "id_j", stringsAsFactors = FALSE)
  w <- tournament_identity_wins(t, ids, fake_factions)
  expect_equal(w$identity[w$side == "Runner"], "id_a1")
})

test_that("a wrong-side identity is dropped from the breakdown too", {
  # The totals already drop these; a treemap is exactly where one stray
  # card would appear as a faction that cannot be on that side.
  ids <- data.frame(code = c("id_a", "id_hb", "id_j"),
                    title = c("Hoshiko", "Jeeves", "Palana"),
                    faction_code = c("anarch", "haas-bioroid", "jinteki"),
                    stringsAsFactors = FALSE)
  t <- data.frame(winner_runner_identity = c("id_a", "id_hb"),
                  winner_corp_identity = c("id_j", "id_j"),
                  stringsAsFactors = FALSE)
  w <- tournament_identity_wins(t, ids, fake_factions)
  expect_equal(w$identity[w$side == "Runner"], "Hoshiko")
})

test_that("the treemap nests side, faction and identity", {
  ids <- data.frame(
    code = c("id_a1", "id_a2", "id_j"),
    title = c("Hoshiko", "Reina", "Palana"),
    faction_code = c("anarch", "anarch", "jinteki"),
    stringsAsFactors = FALSE
  )
  t <- data.frame(winner_runner_identity = c("id_a1", "id_a1", "id_a2"),
                  winner_corp_identity = rep("id_j", 3),
                  stringsAsFactors = FALSE)
  h <- faction_treemap_hierarchy(tournament_identity_wins(t, ids, fake_factions))

  expect_equal(vapply(h$children, function(x) x$name, character(1)),
               c("Runner", "Corp"))

  anarch <- h$children[[1]]$children[[1]]
  # Faction level carries its share of the side; identity level carries
  # raw wins, because a share of a share of a half is a number nobody
  # can hold.
  expect_equal(anarch$name, "Anarch (100.0%)")
  expect_equal(anarch$color, unname(FACTION_COLOURS[["anarch"]]))
  # Count AND share, because a percentage alone hides how much is behind
  # it: 50% of a faction with four wins and 50% of one with four hundred
  # are not the same claim. The share is of the FACTION, which is the box
  # the reader just clicked into.
  expect_equal(vapply(anarch$children, function(x) x$name, character(1)),
               c("Hoshiko (2, 66.7%)", "Reina (1, 33.3%)"))
  expect_equal(anarch$children[[1]]$size, 2L)
})

test_that("the treemap returns NULL rather than an empty widget", {
  expect_null(faction_treemap_hierarchy(NULL))
  expect_null(faction_treemap_hierarchy(
    tournament_identity_wins(NULL, NULL)
  ))
})
