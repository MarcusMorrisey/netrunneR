tournaments_fixture <- data.frame(
  # The abr spelling: dot-separated, with a trailing dot. as.Date() reads
  # every one of these as NA, silently.
  date = c("2019.05.04.", "2021.09.11.", "2024.02.17.", "2026.08.27."),
  winner_runner_identity = c("id_a", "id_a", "id_c", "id_s"),
  winner_corp_identity = c("id_j", "id_j", "id_hb", "id_nbn"),
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

  # And a bad row among good ones drops that row rather than the release.
  mixed <- data.frame(date = c("2019.05.04.", "garbage", "2026.08.27."),
                      stringsAsFactors = FALSE)
  expect_equal(date_bounds(with_parsed_dates(mixed)),
               as.Date(c("2019-05-04", "2026-08-27")))
})

test_that("filter_by_date keeps the range inclusive at both ends", {
  d <- with_parsed_dates(tournaments_fixture)
  kept <- filter_by_date(d, as.Date(c("2019-05-04", "2021-09-11")))
  expect_equal(nrow(kept), 2L)

  expect_equal(nrow(filter_by_date(d, NULL)), 4L)
  expect_null(filter_by_date(NULL, NULL))
})

test_that("filter_by_date drops rows whose date would not parse", {
  d <- with_parsed_dates(data.frame(date = c("2019.05.04.", "garbage"),
                                    stringsAsFactors = FALSE))
  expect_equal(nrow(filter_by_date(d, as.Date(c("2000-01-01", "2030-01-01")))), 1L)
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

test_that("the filter reports the full range before the slider exists", {
  # Every downstream reactive runs once before renderUI has produced the
  # slider. Without the fallback that first pass draws nothing and the
  # view flickers from empty to full.
  bounds <- date_bounds(with_parsed_dates(tournaments_fixture))
  shiny::testServer(
    mod_date_filter_server,
    args = list(bounds = bounds, periods = rotation_periods(NULL)),
    {
      expect_equal(session$getReturned()(), bounds)
    }
  )
})

test_that("moving the slider changes what the filter reports", {
  bounds <- date_bounds(with_parsed_dates(tournaments_fixture))
  shiny::testServer(
    mod_date_filter_server,
    args = list(bounds = bounds, periods = rotation_periods(NULL)),
    {
      session$setInputs(dates = as.Date(c("2021-01-01", "2022-01-01")))
      expect_equal(session$getReturned()(),
                   as.Date(c("2021-01-01", "2022-01-01")))
    }
  )
})

test_that("a preset applies its range immediately", {
  # THE CONTRACT CHANGED when the filter became app-level. It used to be
  # that a preset only nudged the slider and the browser echoed the new
  # value back; the range now lives in a reactiveVal, because the slider
  # is destroyed every time the reader switches view and a range that
  # only exists in the widget does not survive that.
  #
  # So a preset sets the state AND moves the slider. The state is what
  # every figure reads, which is what this asserts. That the slider moves
  # too is only observable in a browser -- updateSliderInput() is a no-op
  # under testServer, which neither applies it to `input` nor records it
  # anywhere reachable.
  bounds <- date_bounds(with_parsed_dates(tournaments_fixture))
  rotation <- data.frame(
    code = c("r1", "r2"), name = c("First rotation", "Second rotation"),
    date_start = c("2017-01-01", "2020-01-01"), stringsAsFactors = FALSE
  )
  periods <- rotation_periods(rotation, max_date = bounds[[2]])

  shiny::testServer(
    mod_date_filter_server,
    args = list(bounds = bounds, periods = periods),
    {
      session$setInputs(dates = as.Date(c("2021-01-01", "2022-01-01")))
      expect_equal(session$getReturned()(), as.Date(c("2021-01-01", "2022-01-01")))

      # Newest first, so period 1 is "Second rotation": 2020-01-01 to the
      # end of the data.
      session$setInputs(preset_1 = 1)
      expect_equal(session$getReturned()(), c(as.Date("2020-01-01"), bounds[[2]]))

      session$setInputs(preset_all = 1)
      expect_equal(session$getReturned()(), bounds)
    }
  )
})

test_that("the range survives the slider being destroyed", {
  # Switching view tears the slider out of the DOM and rebuilds it. The
  # selection is held in a reactiveVal precisely so that navigating does
  # not silently reset the reader's filter to all-time.
  bounds <- date_bounds(with_parsed_dates(tournaments_fixture))
  shiny::testServer(
    mod_date_filter_server,
    args = list(bounds = bounds, periods = rotation_periods(NULL)),
    {
      session$setInputs(dates = as.Date(c("2021-01-01", "2022-01-01")))
      expect_equal(session$getReturned()(), as.Date(c("2021-01-01", "2022-01-01")))

      # A rebuilt slider re-renders from the remembered range, not from
      # the bounds. sliderInput() writes dates as epoch MILLISECONDS into
      # data-from/data-to, so that is what to look for -- the ISO string
      # never appears in the markup at all.
      ms <- function(d) sprintf("%.0f", as.numeric(as.Date(d)) * 86400000)
      html <- as.character(output$filter$html)
      expect_match(html, sprintf('data-from="%s"', ms("2021-01-01")), fixed = TRUE)
      expect_match(html, sprintf('data-to="%s"', ms("2022-01-01")), fixed = TRUE)
      # The bounds are still the ENDS of the track; only the handles moved.
      expect_match(html, sprintf('data-min="%s"', ms("2019-05-04")), fixed = TRUE)
    }
  )
})

test_that("the collapsed filter still says what it is filtering to", {
  # The panel collapses; the answer does not. A collapsed filter that
  # does not name its range turns every chart under it into a number with
  # an invisible caveat.
  bounds <- date_bounds(with_parsed_dates(tournaments_fixture))
  shiny::testServer(
    mod_date_filter_server,
    args = list(bounds = bounds, periods = rotation_periods(NULL)),
    {
      expect_equal(output$range_label, "May 2019 \u2014 Aug 2026")
      session$setInputs(dates = as.Date(c("2021-01-01", "2022-06-01")))
      expect_equal(output$range_label, "Jan 2021 \u2014 Jun 2022")

      # And the summary row carries it, so it is visible while collapsed.
      html <- as.character(output$filter$html)
      expect_match(html, "nr-filter-summary", fixed = TRUE)
      expect_match(html, "range_label", fixed = TRUE)
      # Open by default: a filter nobody can see is a filter nobody uses.
      expect_match(html, "<details", fixed = TRUE)
      expect_match(html, "open", fixed = TRUE)
    }
  )
})

test_that("the rotation shortcuts are named, newest first", {
  rotation <- data.frame(
    code = c("r1", "r2"), name = c("First rotation", "Second rotation"),
    date_start = c("2017-01-01", "2020-01-01"), stringsAsFactors = FALSE
  )
  periods <- rotation_periods(rotation, max_date = as.Date("2026-08-27"))
  expect_equal(periods$label,
               c("Second rotation", "First rotation", "Before first rotation"))
})

test_that("no bounds means no filter and no invented range", {
  shiny::testServer(
    mod_date_filter_server,
    args = list(bounds = NULL, periods = rotation_periods(NULL)),
    {
      expect_null(session$getReturned()())
      expect_null(output$filter$html)
    }
  )
})
