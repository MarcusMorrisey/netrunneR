test_that("parse_deck_ref() accepts a bare numeric id", {
  expect_identical(parse_deck_ref("12345"), "12345")
})

test_that("parse_deck_ref() parses a decklist URL with a trailing title slug to the same id as the bare number", {
  url <- "https://netrunnerdb.com/en/decklist/12345/some-deck-title"
  expect_identical(parse_deck_ref(url), parse_deck_ref("12345"))
})

test_that("parse_deck_ref() parses deck/view/ and deck/ URLs", {
  expect_identical(parse_deck_ref("https://netrunnerdb.com/en/deck/view/12345"), "12345")
  expect_identical(parse_deck_ref("https://netrunnerdb.com/en/deck/12345/title-slug"), "12345")
})

test_that("parse_deck_ref() yields NULL for NULL or empty input", {
  expect_null(parse_deck_ref(NULL))
  expect_null(parse_deck_ref(""))
})

test_that("fetch_deck() yields NULL for a NULL ref without issuing a request", {
  expect_null(fetch_deck(NULL, list(base_url = "https://example.test/api", pacing = list(min_delay_s = 1, max_delay_s = 2))))
})

test_that("fetch_deck() returns the parsed deck from a well-formed envelope", {
  withr::local_envvar(NRDB_CONTACT = "fixture@example.test")

  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
      list(
        success = TRUE,
        total = 1L,
        data = list(list(
          id = 12345L,
          name = "Fixture Deck",
          identity = "id01",
          cards = list(`ice01` = 3L, `brk01` = 2L)
        ))
      ),
      auto_unbox = TRUE
    )))
  })

  li <- list(base_url = "https://example.test/api", pacing = list(min_delay_s = 1, max_delay_s = 2))
  deck <- fetch_deck("12345", li)

  expect_identical(deck$name, "Fixture Deck")
  expect_identical(deck$identity_code, "id01")
  expect_identical(unname(deck$cards[["ice01"]]), 3L)
  expect_identical(unname(deck$cards[["brk01"]]), 2L)
})

test_that("fetch_deck() refuses an envelope with success FALSE, naming the deck id", {
  withr::local_envvar(NRDB_CONTACT = "fixture@example.test")

  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
      list(success = FALSE, total = 0L, data = list()),
      auto_unbox = TRUE
    )))
  })

  li <- list(base_url = "https://example.test/api", pacing = list(min_delay_s = 1, max_delay_s = 2))
  expect_error(fetch_deck("99999", li), regexp = "99999")
})

test_that("fetch_deck() refuses an envelope with total 0", {
  withr::local_envvar(NRDB_CONTACT = "fixture@example.test")

  httr2::local_mocked_responses(function(req) {
    httr2::response(status_code = 200, body = charToRaw(jsonlite::toJSON(
      list(success = TRUE, total = 0L, data = list()),
      auto_unbox = TRUE
    )))
  })

  li <- list(base_url = "https://example.test/api", pacing = list(min_delay_s = 1, max_delay_s = 2))
  expect_error(fetch_deck("55555", li), regexp = "55555")
})

test_that("check_deck_envelope() refuses when data is present but empty", {
  expect_error(check_deck_envelope(list(success = TRUE, total = 1L, data = list()), "7"), regexp = "7")
})
