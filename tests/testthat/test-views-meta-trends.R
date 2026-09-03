trend_identities <- data.frame(
  code = c("id_a", "id_c", "id_s", "id_hb", "id_j", "id_nbn"),
  title = c("Hoshiko", "Steve", "Zahya", "Jeeves", "Palana", "Sportsmetal"),
  faction_code = c("anarch", "criminal", "shaper",
                   "haas-bioroid", "jinteki", "nbn"),
  stringsAsFactors = FALSE
)

trend_factions <- data.frame(
  code = c("anarch", "criminal", "shaper", "haas-bioroid", "jinteki", "nbn"),
  name = c("Anarch", "Criminal", "Shaper", "Haas-Bioroid", "Jinteki", "NBN"),
  side = c("runner", "runner", "runner", "corp", "corp", "corp"),
  stringsAsFactors = FALSE
)

# Q1 2024: two Anarch, one Criminal. Q3 2024: two Criminal, nothing else.
trend_tournaments <- with_parsed_dates(data.frame(
  date = c("2024.01.10.", "2024.02.20.", "2024.03.05.",
           "2024.07.10.", "2024.08.20."),
  winner_runner_identity = c("id_a", "id_a", "id_c", "id_c", "id_c"),
  winner_corp_identity = c("id_j", "id_j", "id_hb", "id_nbn", "id_nbn"),
  stringsAsFactors = FALSE
))

test_that("period_start floors to the quarter", {
  expect_equal(period_start(as.Date(c("2024-01-10", "2024-03-31",
                                      "2024-04-01")), "quarter"),
               as.Date(c("2024-01-01", "2024-01-01", "2024-04-01")))
})

test_that("quarterly shares zero-fill the periods a faction missed", {
  # Without a zero row, geom_area interpolates straight across the gap
  # and draws a faction as continuously present through quarters it
  # never appeared in.
  q <- faction_quarterly_shares(trend_tournaments, trend_identities,
                                trend_factions)

  expect_setequal(unique(q$period), as.Date(c("2024-01-01", "2024-07-01")))

  anarch_q3 <- q[q$faction_code == "anarch" & q$period == as.Date("2024-07-01"), ]
  expect_equal(nrow(anarch_q3), 1L)
  expect_equal(anarch_q3$wins, 0L)
})

test_that("shares are within side and per period", {
  q <- faction_quarterly_shares(trend_tournaments, trend_identities,
                                trend_factions)
  q1r <- q[q$side == "Runner" & q$period == as.Date("2024-01-01"), ]
  expect_equal(sum(q1r$share), 1)
  expect_equal(q1r$share[q1r$faction_code == "anarch"], 2 / 3)
})

test_that("a faction with no wins has NO rank, so its line breaks", {
  # Ranking every faction every quarter gives the ones on zero an
  # ordering among themselves -- and that ordering moves, so the bump
  # chart shows lines crossing that encode nothing but the tie-break.
  q <- faction_quarterly_shares(trend_tournaments, trend_identities,
                                trend_factions)

  anarch_q3 <- q[q$faction_code == "anarch" & q$period == as.Date("2024-07-01"), ]
  expect_true(is.na(anarch_q3$rank))

  crim_q3 <- q[q$faction_code == "criminal" & q$period == as.Date("2024-07-01"), ]
  expect_equal(crim_q3$rank, 1L)
})

test_that("ranks are dense from 1 among the factions that did win", {
  q <- faction_quarterly_shares(trend_tournaments, trend_identities,
                                trend_factions)
  q1r <- q[q$side == "Runner" & q$period == as.Date("2024-01-01"), ]
  ranked <- sort(q1r$rank[!is.na(q1r$rank)])
  expect_equal(ranked, seq_along(ranked))
  expect_equal(q1r$faction_code[which(q1r$rank == 1L)], "anarch")
})

test_that("the period grid comes from the data, not from a seq()", {
  # A filter selecting two dates years apart must not manufacture every
  # empty quarter between them.
  far <- with_parsed_dates(data.frame(
    date = c("2015.01.10.", "2024.01.10."),
    winner_runner_identity = c("id_a", "id_c"),
    winner_corp_identity = c("id_j", "id_j"), stringsAsFactors = FALSE
  ))
  q <- faction_quarterly_shares(far, trend_identities, trend_factions)
  expect_equal(length(unique(q$period)), 2L)
})

test_that("quarterly shares survive empty and unparsed input", {
  expect_equal(nrow(faction_quarterly_shares(NULL, trend_identities)), 0L)
  expect_equal(nrow(faction_quarterly_shares(trend_tournaments, NULL)), 0L)
  # No .date column at all: the caller has not run with_parsed_dates().
  raw <- data.frame(date = "2024.01.10.", winner_runner_identity = "id_a",
                    winner_corp_identity = "id_j", stringsAsFactors = FALSE)
  expect_equal(nrow(faction_quarterly_shares(raw, trend_identities)), 0L)
})

test_that("the area chart is plotly, and actually carries traces", {
  # THE BUG THIS EXISTS FOR: the area was a ggplot handed to ggplotly(),
  # and ggplotly does not implement geom_area(position = "fill"). It does
  # not say so either -- it returned a figure with the axes, the facet
  # strips and the legend all correct, and ZERO traces, which renders as
  # a properly labelled blank panel. Counting traces is the only
  # assertion that would have caught it.
  skip_if_not_installed("plotly")
  q <- faction_quarterly_shares(trend_tournaments, trend_identities,
                                trend_factions)

  area <- build_faction_area(q)
  expect_s3_class(area, "plotly")

  traces <- plotly::plotly_build(area)$x$data
  expect_gt(length(traces), 0L)
  # Every faction that won something on either side gets a band.
  expect_equal(length(traces), nrow(unique(q[q$wins > 0, c("side", "faction_code")])))
  # Stacked and normalised by plotly rather than by ggplot beforehand.
  expect_true(all(vapply(traces, function(t) identical(t$stackgroup, "one"),
                         logical(1))))
})

test_that("the bump chart is a ggplot with the unranked rows dropped", {
  skip_if_not_installed("ggplot2")
  q <- faction_quarterly_shares(trend_tournaments, trend_identities,
                                trend_factions)
  bump <- build_faction_bump(q)
  expect_s3_class(bump, "ggplot")
  expect_lt(nrow(bump$data), nrow(q))
  expect_false(any(is.na(bump$data$rank)))
})

test_that("the trend charts return NULL rather than an empty plot", {
  expect_null(build_faction_area(NULL))
  expect_null(build_faction_bump(NULL))
  empty <- faction_quarterly_shares(NULL, NULL)
  expect_null(build_faction_area(empty))
  expect_null(build_faction_bump(empty))
})

# ---- the chord matrix -------------------------------------------------

test_that("the pairing matrix is Corp rows by Runner columns", {
  p <- faction_pairing_matrix(trend_tournaments, trend_identities,
                              trend_factions)
  expect_equal(dim(p$matrix), c(length(p$corp), length(p$runner)))
  expect_equal(sum(p$matrix), 5L)

  # Two Anarch/Jinteki wins in Q1.
  expect_equal(p$matrix[match("jinteki", p$corp), match("anarch", p$runner)], 2L)
})

test_that("only factions that actually appear get a spoke", {
  # An axis of empty spokes is a ring of labels attached to nothing.
  t <- with_parsed_dates(data.frame(
    date = "2024.01.10.", winner_runner_identity = "id_a",
    winner_corp_identity = "id_j", stringsAsFactors = FALSE
  ))
  p <- faction_pairing_matrix(t, trend_identities, trend_factions)
  expect_equal(p$runner, "anarch")
  expect_equal(p$corp, "jinteki")
})

test_that("a pairing needs BOTH winners, on the right sides", {
  t <- with_parsed_dates(data.frame(
    date = c("2024.01.10.", "2024.01.11.", "2024.01.12."),
    # a missing corp winner, then a Corp identity in the Runner slot
    winner_runner_identity = c("id_a", "id_a", "id_hb"),
    winner_corp_identity = c("id_j", "", "id_j"),
    stringsAsFactors = FALSE
  ))
  p <- faction_pairing_matrix(t, trend_identities, trend_factions)
  expect_equal(sum(p$matrix), 1L)
})

test_that("the two Neutrals get distinguishable spokes", {
  # A chord ring with two identical labels is unreadable.
  ids <- rbind(trend_identities, data.frame(
    code = c("id_nr", "id_nc"), title = c("The Masque", "The Shadow"),
    faction_code = c("neutral-runner", "neutral-corp"),
    stringsAsFactors = FALSE
  ))
  facs <- rbind(trend_factions, data.frame(
    code = c("neutral-runner", "neutral-corp"), name = c("Neutral", "Neutral"),
    side = c("runner", "corp"), stringsAsFactors = FALSE
  ))
  t <- with_parsed_dates(data.frame(
    date = "2024.01.10.", winner_runner_identity = "id_nr",
    winner_corp_identity = "id_nc", stringsAsFactors = FALSE
  ))
  p <- faction_pairing_matrix(t, ids, facs)
  expect_equal(p$runner_names, "Neutral (R)")
  expect_equal(p$corp_names, "Neutral (C)")
})

test_that("the pairing matrix is NULL when there is nothing to draw", {
  expect_null(faction_pairing_matrix(NULL, trend_identities, trend_factions))
  expect_null(faction_pairing_matrix(trend_tournaments, trend_identities, NULL))
})
