mini_tournaments <- function() {
  data.frame(
    id = as.character(1:8),
    title = "Event",
    # abr writes dates dot-separated WITH a trailing dot.
    date = c("2015.05.01.", "2018.03.01.", "2019.06.01.", "2022.01.01.",
             "2024.02.01.", "2026.01.01.", "2026.02.01.", "2016.07.01."),
    format = "standard",
    location_state = NA_character_,
    location_country = c("United States", "United States", "United Kingdom",
                         "Germany", "Germany", "Germany", "Singapore", ""),
    location_lat = c(40.7, 40.7, 51.5, 52.5, 52.5, 48.1, 1.35, NA),
    location_lng = c(-74.0, -74.0, -0.12, 13.4, 13.4, 11.6, 103.8, NA),
    players_count = c(10L, 20L, 30L, 40L, NA, 5L, 8L, 12L),
    top_count = 4L,
    winner_runner_identity = "35013",
    winner_corp_identity = "35068",
    stringsAsFactors = FALSE
  )
}

test_that("abr's dotted date format parses, including the trailing dot", {
  # as.Date() returns NA for "2026.08.27." -- silently, for every row --
  # so a filter built on it would match nothing and read as a data
  # problem rather than a parsing one.
  got <- parse_abr_date(c("2026.08.27.", "2014.08.10.", "nonsense"))

  expect_equal(got[[1]], as.Date("2026-08-27"))
  expect_equal(got[[2]], as.Date("2014-08-10"))
  expect_true(is.na(got[[3]]))
})

test_that("countries are counted, and rows with no country are dropped", {
  s <- tournament_country_counts(mini_tournaments())

  # 8 rows, one of which has no country at all.
  expect_equal(sum(s$counts$tournaments), 7)
  expect_false("" %in% s$counts$name)
})

test_that("the United States is respelled to match the map", {
  # The single measured difference between what abr records and what the
  # world map calls the same country. Without it the country with the
  # most tournaments in the data vanishes from the map.
  s <- tournament_country_counts(mini_tournaments())

  expect_true("United States of America" %in% s$counts$name)
  expect_false("United States" %in% s$counts$name)
})

test_that("countries the map cannot place are reported, not dropped silently", {
  # A country that fails to join looks exactly like a country with no
  # tournaments once it is drawn, and only one of those is true.
  s <- tournament_country_counts(mini_tournaments(),
                                 map_names = c("United States of America",
                                               "United Kingdom", "Germany"))

  expect_equal(s$unmatched, "Singapore")
})

test_that("country names that already match are left alone", {
  # An earlier version guessed renames for Czechia and South Korea, and
  # both guesses BROKE countries that matched perfectly on their own.
  expect_equal(country_map_name(c("Czechia", "South Korea", "Germany")),
               c("Czechia", "South Korea", "Germany"))
})

test_that("venues are grouped by rounded coordinate, not one point per event", {
  # A shop hosting several events is one bubble sized several, not
  # several bubbles stacked on the same pixel.
  v <- tournament_venues(mini_tournaments())

  expect_equal(sum(v$count), 7)          # the coordinate-less row is out
  expect_equal(max(v$count), 2)          # New York twice, Berlin twice
  expect_true(all(c("location_lat", "location_lng", "count") %in% names(v)))
})

test_that("a release with no coordinate columns yields no venues rather than an error", {
  # Coordinates were admitted to the abr allowlist after this store had
  # been built, and a release is promoted independently of the package
  # reading it. The map then draws its country layer alone, which is a
  # weaker map and not a broken one.
  old <- mini_tournaments()
  old$location_lat <- NULL
  old$location_lng <- NULL

  expect_equal(nrow(tournament_venues(old)), 0)
})

test_that("rotation periods run from each start to the day before the next", {
  rot <- data.frame(
    code = c("r1", "r2"), name = c("First Rotation", "Second Rotation"),
    date_start = c("2017-10-01", "2018-12-21"), stringsAsFactors = FALSE
  )
  p <- rotation_periods(rot, max_date = as.Date("2026-08-29"))

  # Newest first, plus the span before the first rotation.
  expect_equal(p$label, c("Second Rotation", "First Rotation",
                          "Before first rotation"))
  second <- p[p$label == "Second Rotation", ]
  first <- p[p$label == "First Rotation", ]
  expect_equal(first$end, as.Date("2018-12-20"))
  expect_equal(second$start, as.Date("2018-12-21"))
  # The most recent period runs to the end of the data, not to its own
  # start date.
  expect_equal(second$end, as.Date("2026-08-29"))
})

test_that("the span before the first rotation is included", {
  # A third of this data predates 2017, and a filter that cannot reach
  # it would hide it.
  rot <- data.frame(code = "r1", name = "First Rotation",
                    date_start = "2017-10-01", stringsAsFactors = FALSE)
  p <- rotation_periods(rot, max_date = as.Date("2026-08-29"))

  pre <- p[p$label == "Before first rotation", ]
  expect_equal(nrow(pre), 1)
  expect_equal(pre$end, as.Date("2017-09-30"))
})

test_that("no rotation table yields no periods rather than invented ones", {
  # There are seven rotations and their dates are not guessable.
  expect_equal(nrow(rotation_periods(NULL)), 0)
  expect_equal(nrow(rotation_periods(data.frame())), 0)
})
