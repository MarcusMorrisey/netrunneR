# Covers R/mod_deck_compare.R: the Compare button's fetch/resolve/pair pipeline, the
# unresolved-codes caption, and DL-040's leave-prior-result-standing behaviour on a
# refused fetch or refused deck.

deck_compare_args <- function() {
  all_codes <- mini_pool_cardpool()[c("code", "title", "type_code")]
  list(
    all_codes = all_codes,
    cards = mini_pool_cardpool(),
    matchup = compute_ice_breaker_matchups(
      mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
    )$matchups,
    cardpool_release_id = "2026-01-01"
  )
}

fixture_deck <- function(cards) {
  # Identity code "id01" folded into `cards` like any other card, at
  # quantity 1 -- matches fetch_deck()'s real shape now that the identity
  # is found via type_code == "identity" rather than a fetched field (DL-045).
  list(id = 1, name = "Fixture Deck", cards = c(cards, id01 = 1L))
}

test_that("a successful compare populates the result reactive with the expected pair count", {
  testthat::local_mocked_bindings(
    fetch_deck = function(ref, lineage) {
      if (identical(ref, "corp")) {
        fixture_deck(c(ice01 = 3L))
      } else {
        fixture_deck(c(brk01 = 1L, brk04 = 1L))
      }
    }
  )

  shiny::testServer(
    mod_deck_compare_server,
    args = deck_compare_args(),
    {
      session$setInputs(corp_ref = "corp", runner_ref = "runner")
      session$setInputs(compare = 1)
      session$flushReact()

      r <- result()
      expect_false(is.null(r))
      expect_equal(nrow(r$pairing$matchups), 2L)
    }
  )
})

test_that("a deck carrying an unresolved code still populates results and the unresolved caption", {
  testthat::local_mocked_bindings(
    fetch_deck = function(ref, lineage) {
      if (identical(ref, "corp")) {
        fixture_deck(c(ice01 = 3L, zzz99 = 1L))
      } else {
        fixture_deck(c(brk01 = 1L))
      }
    }
  )

  shiny::testServer(
    mod_deck_compare_server,
    args = deck_compare_args(),
    {
      session$setInputs(corp_ref = "corp", runner_ref = "runner")
      session$setInputs(compare = 1)
      session$flushReact()

      r <- result()
      expect_false(is.null(r))
      expect_true(nrow(r$pairing$matchups) > 0)

      caption <- as.character(output$unresolved_caption$html)
      expect_match(caption, "zzz99", fixed = TRUE)
      expect_match(caption, "2026-01-01", fixed = TRUE)
    }
  )
})

test_that("a refused fetch leaves the result reactive at its prior value and populates status", {
  # DL-040: Compare is an explicit press against free-text references, so a
  # typo should not destroy a comparison the user is currently reading.
  # First set up a successful result, then trigger a refused fetch and
  # assert the result reactive is unchanged from the first press.
  call_count <- 0
  testthat::local_mocked_bindings(
    fetch_deck = function(ref, lineage) {
      call_count <<- call_count + 1
      if (call_count <= 2) {
        if (identical(ref, "corp")) fixture_deck(c(ice01 = 3L)) else fixture_deck(c(brk01 = 1L))
      } else {
        NULL
      }
    }
  )

  shiny::testServer(
    mod_deck_compare_server,
    args = deck_compare_args(),
    {
      session$setInputs(corp_ref = "corp", runner_ref = "runner")
      session$setInputs(compare = 1)
      session$flushReact()

      first_result <- result()
      expect_false(is.null(first_result))

      session$setInputs(compare = 2)
      session$flushReact()

      expect_identical(result(), first_result)
      expect_false(is.null(status_message()))
    }
  )
})

test_that("suite_nav_ui(deckCompare) marks the item active with no inert class", {
  rendered <- as.character(suite_nav_ui("deckCompare"))
  expect_match(rendered, "Deck Compare")
  expect_match(rendered, "nr-suite-item-active")
  expect_no_match(rendered, "nr-suite-item-inert")
})

test_that("unresolved_codes_caption() returns NULL when nothing is unresolved", {
  expect_null(unresolved_codes_caption(character(0), "2026-01-01"))
})

test_that("unresolved_codes_caption() uses the release_info inline style", {
  rendered <- as.character(unresolved_codes_caption(c("zzz99"), "2026-01-01"))
  expect_match(rendered, "font-size: 11px; color: #8a8f98;", fixed = TRUE)
})
