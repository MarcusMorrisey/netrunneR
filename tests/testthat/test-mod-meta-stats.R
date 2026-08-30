stats_identities <- data.frame(
  code = c("id_a", "id_c", "id_s", "id_hb", "id_j", "id_nbn"),
  faction_code = c("anarch", "criminal", "shaper",
                   "haas-bioroid", "jinteki", "nbn"),
  stringsAsFactors = FALSE
)

stats_factions <- data.frame(
  code = c("anarch", "criminal", "shaper", "haas-bioroid", "jinteki", "nbn"),
  name = c("Anarch", "Criminal", "Shaper", "Haas-Bioroid", "Jinteki", "NBN"),
  side = c("runner", "runner", "runner", "corp", "corp", "corp"),
  stringsAsFactors = FALSE
)

stats_tournaments <- data.frame(
  date = c("2019.05.04.", "2019.06.01.", "2024.02.17.", "2026.08.27.", "2026.08.28."),
  winner_runner_identity = c("id_a", "id_a", "id_c", "id_s", ""),
  winner_corp_identity = c("id_j", "id_j", "id_hb", "id_nbn", ""),
  stringsAsFactors = FALSE
)

test_that("the stats view shapes its data and says how much is charted", {
  shiny::testServer(
    mod_meta_stats_server,
    args = list(tournaments = stats_tournaments, identities = stats_identities,
                factions = stats_factions),
    {
      session$flushReact()
      s <- shaped()
      expect_equal(sum(s$wins$wins[s$wins$side == "Runner"]), 4L)
      expect_equal(s$undecided, 1L)
      expect_match(as.character(output$summary$html), "4 tournaments")
      expect_match(as.character(output$summary$html), "1 more concluded")
    }
  )
})

test_that("the date filter narrows the charts", {
  shiny::testServer(
    mod_meta_stats_server,
    args = list(tournaments = stats_tournaments, identities = stats_identities,
                factions = stats_factions),
    {
      session$flushReact()
      expect_equal(nrow(in_range()), 5L)

      session$setInputs(`dates-dates` = as.Date(c("2019-01-01", "2019-12-31")))
      expect_equal(nrow(in_range()), 2L)
      s <- shaped()
      # Only Anarch won inside that window.
      expect_equal(s$wins$faction_code[s$wins$side == "Runner"], "anarch")
    }
  )
})

test_that("no abr release renders a message, not an empty chart", {
  shiny::testServer(
    mod_meta_stats_server,
    args = list(tournaments = NULL, identities = stats_identities),
    {
      session$flushReact()
      expect_null(shaped())
      expect_match(as.character(output$treemap_slot$html), "nothing to chart")
      expect_match(as.character(output$waffle_slot$html), "nothing to chart")
    }
  )
})

test_that("an unknown faction is named rather than passed off as grey", {
  ids <- rbind(stats_identities,
               data.frame(code = "id_x", faction_code = "brand-new",
                          stringsAsFactors = FALSE))
  t <- stats_tournaments
  t$winner_runner_identity[[1]] <- "id_x"
  shiny::testServer(
    mod_meta_stats_server,
    args = list(tournaments = t, identities = ids, factions = stats_factions),
    {
      session$flushReact()
      expect_match(as.character(output$notes$html), "brand-new")
    }
  )
})

test_that("nothing is flagged when every faction has a colour", {
  shiny::testServer(
    mod_meta_stats_server,
    args = list(tournaments = stats_tournaments, identities = stats_identities,
                factions = stats_factions),
    {
      session$flushReact()
      expect_null(output$notes$html)
    }
  )
})

test_that("the waffle builds when ggplot2 is available", {
  skip_if_not_installed("ggplot2")
  wins <- tournament_faction_wins(stats_tournaments, stats_identities,
                                  stats_factions)$wins
  p <- build_faction_waffle(wins)
  expect_s3_class(p, "ggplot")
  # 100 squares per side, and the plot draws exactly what
  # faction_waffle_squares() produced.
  expect_equal(nrow(p$data), 200L)
})

test_that("the nav strip offers all three views", {
  # Rendered HTML escapes the apostrophes in the onclick handler, so the
  # match is on the escaped form -- matching the R source spelling passes
  # nothing and looks like a missing link.
  html <- as.character(suite_nav_ui("metaStats"))
  expect_match(html, "nav_view&#39;, &#39;iceBreaker&#39;", fixed = TRUE)
  expect_match(html, "nav_view&#39;, &#39;metaMaps&#39;", fixed = TRUE)
  # The active one is a label, not a link to itself.
  expect_false(grepl("&#39;metaStats&#39;", html, fixed = TRUE))
  expect_no_match(html, "nr-suite-item-inert")
})


test_that("a wrong-side winner is named under the charts", {
  ids <- rbind(stats_identities,
               data.frame(code = "id_bad", faction_code = "haas-bioroid",
                          stringsAsFactors = FALSE))
  t <- stats_tournaments
  t$winner_runner_identity[[1]] <- "id_bad"
  shiny::testServer(
    mod_meta_stats_server,
    args = list(tournaments = t, identities = ids, factions = stats_factions),
    {
      session$flushReact()
      expect_equal(shaped()$misfiled, 1L)
      expect_match(as.character(output$misfiled$html), "other side")
    }
  )
})

test_that("nothing is said about wrong sides when there are none", {
  shiny::testServer(
    mod_meta_stats_server,
    args = list(tournaments = stats_tournaments, identities = stats_identities,
                factions = stats_factions),
    {
      session$flushReact()
      expect_equal(shaped()$misfiled, 0L)
      expect_null(output$misfiled$html)
    }
  )
})
