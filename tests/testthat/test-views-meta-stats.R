test_that("largest_remainder always totals exactly, and is deterministic", {
  # The point of the function. Independent rounding gives 98 here.
  shares <- c(1, 1, 1) / 3
  expect_equal(sum(largest_remainder(shares, 100L)), 100L)
  expect_equal(largest_remainder(shares, 100L), c(34L, 33L, 33L))

  # Ties break by position, so the same input draws the same chart every
  # time -- a waffle that reshuffles itself between renders is a bug the
  # reader has no way to see.
  expect_equal(largest_remainder(c(0.5, 0.5), 3L), c(2L, 1L))

  expect_equal(largest_remainder(numeric(0)), integer(0))
  expect_equal(sum(largest_remainder(c(0.999, 0.001), 100L)), 100L)
})

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

test_that("the waffle is 100 squares per side, in a 5-row grid", {
  wins <- tournament_faction_wins(fake_tournaments, fake_identities,
                                  fake_factions)$wins
  sq <- faction_waffle_squares(wins)

  expect_equal(as.integer(table(sq$side)[["Runner"]]), 100L)
  expect_equal(as.integer(table(sq$side)[["Corp"]]), 100L)
  expect_equal(sort(unique(sq$row)), 1:5)
  expect_equal(max(sq$column), 20L)

  # Anarch is half of the Runner wins, so half the Runner squares.
  runner <- sq[sq$side == "Runner", ]
  expect_equal(sum(runner$faction_code == "anarch"), 50L)
})

test_that("faction_waffle_squares survives an empty frame", {
  expect_equal(nrow(faction_waffle_squares(NULL)), 0L)
  expect_equal(
    nrow(faction_waffle_squares(tournament_faction_wins(NULL, NULL)$wins)), 0L
  )
})

test_that("the treemap hierarchy nests factions under sides with colours", {
  wins <- tournament_faction_wins(fake_tournaments, fake_identities,
                                  fake_factions)$wins
  h <- faction_treemap_hierarchy(wins)

  expect_equal(length(h$children), 2L)
  expect_equal(vapply(h$children, function(x) x$name, character(1)),
               c("Runner", "Corp"))

  anarch <- h$children[[1]]$children[[1]]
  expect_equal(anarch$size, 2L)
  expect_equal(anarch$color, unname(FACTION_COLOURS[["anarch"]]))
  # Percentages are within side, matching the waffle, so the two charts
  # cannot quote different numbers for the same faction.
  expect_equal(anarch$name, "Anarch (50.0%)")
})

test_that("faction_treemap_hierarchy returns NULL rather than an empty widget", {
  expect_null(faction_treemap_hierarchy(NULL))
  expect_null(faction_treemap_hierarchy(tournament_faction_wins(NULL, NULL)$wins))
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

test_that("a faction too small for one square is reported, not padded", {
  # One square is one percent; a faction under half a percent rounds to
  # nothing. Topping it up to one square would quietly break the only
  # promise the chart makes.
  t <- data.frame(
    winner_runner_identity = c(rep("id_a", 400), "id_s"),
    winner_corp_identity = rep("id_j", 401), stringsAsFactors = FALSE
  )
  res <- tournament_faction_wins(t, fake_identities, fake_factions)
  small <- factions_below_resolution(res$wins)

  expect_equal(small$faction_code, "shaper")
  expect_equal(sum(faction_waffle_squares(res$wins)$side == "Runner"), 100L)
  expect_false("shaper" %in% faction_waffle_squares(res$wins)$faction_code)
})

test_that("factions_below_resolution is empty when everything is drawn", {
  wins <- tournament_faction_wins(fake_tournaments, fake_identities,
                                  fake_factions)$wins
  expect_equal(nrow(factions_below_resolution(wins)), 0L)
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
