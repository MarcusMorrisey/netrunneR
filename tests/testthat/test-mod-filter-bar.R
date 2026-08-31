tournaments_fixture <- data.frame(
  # The abr spelling: dot-separated, with a trailing dot. as.Date() reads
  # every one of these as NA, silently.
  date = c("2019.05.04.", "2021.09.11.", "2024.02.17.", "2026.08.27."),
  type = c("store championship", "GNK / seasonal", "store championship",
           "worlds championship"),
  winner_runner_identity = c("id_a", "id_draft_r", "id_c", "id_s"),
  winner_corp_identity = c("id_j", "id_draft_c", "id_hb", "id_nbn"),
  stringsAsFactors = FALSE
)

identities_fixture <- data.frame(
  code = c("id_a", "id_c", "id_s", "id_hb", "id_j", "id_nbn",
           "id_draft_r", "id_draft_c"),
  faction_code = c("anarch", "criminal", "shaper",
                   "haas-bioroid", "jinteki", "nbn",
                   "neutral-runner", "neutral-corp"),
  stringsAsFactors = FALSE
)

test_that("with_parsed_dates adds a real Date column", {
  d <- with_parsed_dates(tournaments_fixture)
  expect_s3_class(d$.date, "Date")
  expect_equal(d$.date[[1]], as.Date("2019-05-04"))
  expect_false(any(is.na(d$.date)))
})

test_that("with_parsed_dates and date_bounds pass NULL through", {
  expect_null(with_parsed_dates(NULL))
  expect_null(with_parsed_dates(tournaments_fixture[0, ]))
  expect_null(date_bounds(NULL))
})

test_that("date_bounds is the span of what parsed", {
  expect_equal(date_bounds(with_parsed_dates(tournaments_fixture)),
               as.Date(c("2019-05-04", "2026-08-27")))
})

test_that("an unparseable date is NA, not an exception", {
  # parse_abr_date() promised NA and threw instead: as.Date() with no
  # format raises "character string is not in a standard unambiguous
  # format" rather than returning NA, so ONE malformed date anywhere in a
  # release aborted the entire view.
  expect_true(all(is.na(parse_abr_date(c("not a date", "", NA)))))
  junk <- data.frame(date = c("not a date", "also not"), stringsAsFactors = FALSE)
  expect_null(date_bounds(with_parsed_dates(junk)))
})

test_that("a preset is clipped to the data, not to the rotation", {
  # A rotation is not a statement about this dataset: the first starts in
  # 2017 and the data is older, the newest runs to today and the data
  # stops at the last sync. A slider handed a value outside its own
  # min/max shows a range containing nothing, which a reader cannot tell
  # apart from a quiet period.
  bounds <- as.Date(c("2019-05-04", "2026-08-27"))
  wide <- data.frame(start = as.Date("2012-01-01"), end = as.Date("2030-01-01"))
  expect_equal(preset_range(wide, bounds), bounds)

  inside <- data.frame(start = as.Date("2021-01-01"), end = as.Date("2022-01-01"))
  expect_equal(preset_range(inside, bounds),
               as.Date(c("2021-01-01", "2022-01-01")))
})

# ---- apply_tournament_filters ----------------------------------------

test_that("dates filter inclusively at both ends", {
  d <- with_parsed_dates(tournaments_fixture)
  f <- list(dates = as.Date(c("2019-05-04", "2021-09-11")), types = character(0),
            include_draft = TRUE)
  expect_equal(nrow(apply_tournament_filters(d, f, character(0))), 2L)

  expect_equal(nrow(apply_tournament_filters(d, NULL)), 4L)
  expect_null(apply_tournament_filters(NULL, f))
})

test_that("an empty type selection means every type, not none", {
  # Unticking the last chip must not blank the page. A reader clearing a
  # filter is asking to see everything; a chart that vanishes instead
  # reads as a crash.
  d <- with_parsed_dates(tournaments_fixture)
  f <- list(dates = NULL, types = character(0), include_draft = TRUE)
  expect_equal(nrow(apply_tournament_filters(d, f)), 4L)
})

test_that("type chips narrow to the chosen types", {
  d <- with_parsed_dates(tournaments_fixture)
  f <- list(dates = NULL, types = "store championship", include_draft = TRUE)
  expect_equal(nrow(apply_tournament_filters(d, f)), 2L)

  f$types <- c("store championship", "worlds championship")
  expect_equal(nrow(apply_tournament_filters(d, f)), 3L)
})

test_that("a release with no type column ignores the type filter", {
  # `type` was admitted to the abr allowlist after this store had been
  # built, and a release is promoted independently of the package that
  # reads it. Asking for a type a release cannot know about must serve
  # everything, not nothing.
  old <- tournaments_fixture
  old$type <- NULL
  d <- with_parsed_dates(old)
  f <- list(dates = NULL, types = "store championship", include_draft = TRUE)
  expect_equal(nrow(apply_tournament_filters(d, f)), 4L)
})

test_that("draft identities are excluded by default and restorable", {
  d <- with_parsed_dates(tournaments_fixture)
  codes <- draft_identity_codes(identities_fixture)
  expect_setequal(codes, c("id_draft_r", "id_draft_c"))

  out <- apply_tournament_filters(
    d, list(dates = NULL, types = character(0), include_draft = FALSE), codes
  )
  expect_equal(nrow(out), 3L)
  expect_false("id_draft_r" %in% out$winner_runner_identity)

  back <- apply_tournament_filters(
    d, list(dates = NULL, types = character(0), include_draft = TRUE), codes
  )
  expect_equal(nrow(back), 4L)
})

test_that("a draft winner on either side alone is still a draft result", {
  d <- with_parsed_dates(data.frame(
    date = c("2020.01.01.", "2020.01.02."),
    winner_runner_identity = c("id_draft_r", "id_a"),
    winner_corp_identity = c("id_j", "id_draft_c"),
    stringsAsFactors = FALSE
  ))
  codes <- draft_identity_codes(identities_fixture)
  out <- apply_tournament_filters(
    d, list(dates = NULL, types = character(0), include_draft = FALSE), codes
  )
  expect_equal(nrow(out), 0L)
})

test_that("draft_identity_codes is empty without the identity table", {
  # No faction table means no way to know which identities are draft
  # ones, and a guess would silently drop real results.
  expect_equal(draft_identity_codes(NULL), character(0))
  expect_equal(draft_identity_codes(identities_fixture[0, ]), character(0))
})

# ---- tournament_types -------------------------------------------------

test_that("tournament types come back most common first", {
  ty <- tournament_types(tournaments_fixture)
  expect_equal(ty$type[[1]], "store championship")
  expect_equal(ty$n[[1]], 2L)
  expect_equal(nrow(ty), 3L)
})

test_that("abr's dropdown separator is not a tournament type", {
  # One upstream record carries a row of box-drawing characters, which is
  # a horizontal rule that escaped into the data rather than a kind of
  # event.
  t <- tournaments_fixture
  t$type[[1]] <- "──── community events ────"
  ty <- tournament_types(t)
  expect_false(any(grepl("─", ty$type)))
})

test_that("tournament_types is empty when the column is absent", {
  old <- tournaments_fixture
  old$type <- NULL
  expect_equal(nrow(tournament_types(old)), 0L)
  expect_equal(nrow(tournament_types(NULL)), 0L)
})

# ---- the module ------------------------------------------------------

test_that("the bar reports the full range, no types, no drafts, up front", {
  # The first pass of every downstream reactive happens before renderUI
  # has produced a control. Without a populated starting state the views
  # draw nothing and flicker.
  shiny::testServer(
    mod_filter_bar_server,
    args = list(tournaments = tournaments_fixture, identities = identities_fixture),
    {
      f <- session$getReturned()()
      expect_equal(f$dates, as.Date(c("2019-05-04", "2026-08-27")))
      expect_equal(f$types, character(0))
      expect_false(f$include_draft)
    }
  )
})

test_that("moving the slider changes what the bar reports", {
  shiny::testServer(
    mod_filter_bar_server,
    args = list(tournaments = tournaments_fixture, identities = identities_fixture),
    {
      session$setInputs(dates = as.Date(c("2021-01-01", "2022-01-01")))
      expect_equal(session$getReturned()()$dates,
                   as.Date(c("2021-01-01", "2022-01-01")))
    }
  )
})

test_that("chips and the draft switch reach the returned selection", {
  shiny::testServer(
    mod_filter_bar_server,
    args = list(tournaments = tournaments_fixture, identities = identities_fixture),
    {
      session$setInputs(types = "store championship")
      expect_equal(session$getReturned()()$types, "store championship")

      # Clearing every chip sends NULL, and that is a real selection
      # meaning "all types" -- not an absence to be ignored.
      session$setInputs(types = NULL)
      expect_equal(session$getReturned()()$types, character(0))

      session$setInputs(include_draft = TRUE)
      expect_true(session$getReturned()()$include_draft)
      # And the date range is untouched by either.
      expect_equal(session$getReturned()()$dates,
                   as.Date(c("2019-05-04", "2026-08-27")))
    }
  )
})

test_that("a preset applies its range immediately", {
  # The range lives in a reactiveVal because the slider is destroyed
  # whenever the reader switches view. A preset sets that AND moves the
  # slider; only the first is observable here, since updateSliderInput()
  # is a no-op under testServer.
  rotation <- data.frame(
    code = c("r1", "r2"), name = c("First rotation", "Second rotation"),
    date_start = c("2017-01-01", "2020-01-01"), stringsAsFactors = FALSE
  )
  shiny::testServer(
    mod_filter_bar_server,
    args = list(tournaments = tournaments_fixture, rotation = rotation,
                identities = identities_fixture),
    {
      # Newest first, so period 1 is "Second rotation".
      session$setInputs(preset_1 = 1)
      expect_equal(session$getReturned()()$dates,
                   c(as.Date("2020-01-01"), as.Date("2026-08-27")))

      session$setInputs(preset_all = 1)
      expect_equal(session$getReturned()()$dates,
                   as.Date(c("2019-05-04", "2026-08-27")))
    }
  )
})

test_that("the rebuilt bar carries the remembered selection", {
  # Switching view tears the controls out of the DOM and rebuilds them.
  # A slider rebuilt from the bounds would silently reset the filter.
  shiny::testServer(
    mod_filter_bar_server,
    args = list(tournaments = tournaments_fixture, identities = identities_fixture),
    {
      session$setInputs(dates = as.Date(c("2021-01-01", "2022-01-01")))

      # sliderInput() writes dates as epoch MILLISECONDS into
      # data-from/data-to; the ISO string never appears in the markup.
      ms <- function(d) sprintf("%.0f", as.numeric(as.Date(d)) * 86400000)
      html <- as.character(output$filter$html)
      expect_match(html, sprintf('data-from="%s"', ms("2021-01-01")), fixed = TRUE)
      # The bounds are still the ENDS of the track; only the handles moved.
      expect_match(html, sprintf('data-min="%s"', ms("2019-05-04")), fixed = TRUE)
    }
  )
})

test_that("the collapsed bar still says what it is filtering to", {
  # The panel collapses; the answer does not. A collapsed filter that
  # does not name its selection turns every chart under it into a number
  # with an invisible caveat.
  shiny::testServer(
    mod_filter_bar_server,
    args = list(tournaments = tournaments_fixture, identities = identities_fixture),
    {
      expect_equal(output$range_label, "May 2019 — Aug 2026")

      session$setInputs(types = "store championship")
      expect_match(output$range_label, "1 of 3 types")
      session$setInputs(include_draft = TRUE)
      expect_match(output$range_label, "incl. draft")

      html <- as.character(output$filter$html)
      expect_match(html, "nr-filter-summary", fixed = TRUE)
      # Open by default: a filter nobody can see is a filter nobody uses.
      expect_match(html, "<details", fixed = TRUE)
    }
  )
})

test_that("the chips say the column is missing rather than showing none", {
  # An empty chip row reads as "no tournaments match", which is a claim
  # about the data. The truth is a claim about the mirror.
  old <- tournaments_fixture
  old$type <- NULL
  shiny::testServer(
    mod_filter_bar_server,
    args = list(tournaments = old, identities = identities_fixture),
    {
      html <- as.character(output$filter$html)
      expect_match(html, "predates the type column", fixed = TRUE)
    }
  )
})

test_that("no tournaments means no bar and no invented range", {
  shiny::testServer(
    mod_filter_bar_server,
    args = list(tournaments = NULL),
    {
      expect_null(session$getReturned()()$dates)
      expect_null(output$filter$html)
    }
  )
})
