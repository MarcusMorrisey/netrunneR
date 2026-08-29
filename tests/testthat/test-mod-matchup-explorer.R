build_mini_matchup <- function() {
  compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )$matchups
}

# A legality bundle shaped like load_ice_breaker_app_data()$legality,
# covering the mini pool. Deliberately a local copy rather than a shared
# helper: testthat gives each test FILE its own environment, so a
# fixture defined at the top level of test-mod-card-browser.R is not
# reachable here. The two are near-identical by coincidence of the pool
# they describe, not because one is derived from the other.
#
# Every card is in the Standard pool except ice03, which sits in a set no
# pool lists, and ice02, which is banned.
matchup_legality_fixture <- function() {
  list(
    format = tibble::tribble(
      ~id,        ~name,
      "standard", "Standard",
      "startup",  "Startup"
    ),
    format_snapshot = tibble::tribble(
      ~id,          ~format_id, ~date_start,  ~card_pool_id, ~restriction_id, ~is_active,
      "standard_1", "standard", "2024-01-01", "pool_std",    "ban_std",       1L,
      "startup_1",  "startup",  "2024-01-01", "pool_start",  NA_character_,   1L
    ),
    card_pool_set = tibble::tribble(
      ~card_pool_id, ~card_set_id,
      "pool_std",    "set_core",
      "pool_start",  "set_core"
    ),
    restriction_card = tibble::tribble(
      ~restriction_id, ~card_id, ~is_banned, ~is_restricted, ~universal_faction_cost, ~global_penalty, ~points,
      "ban_std",       "ice02",  1L,         NA_integer_,    NA_integer_,             NA_integer_,     NA_integer_
    ),
    printing = tibble::tribble(
      ~code,   ~card_id, ~card_set_id,
      "ice01", "ice01",  "set_core",
      "ice02", "ice02",  "set_core",
      "ice03", "ice03",  "set_outside",
      "ice04", "ice04",  "set_core",
      "brk01", "brk01",  "set_core",
      "brk02", "brk02",  "set_core",
      "brk03", "brk03",  "set_core",
      "brk04", "brk04",  "set_core"
    ),
    card_set = tibble::tribble(
      ~id,           ~legacy_code,
      "set_core",    "core",
      "set_outside", "outside"
    )
  )
}

# Named arguments only, and no `...`: an earlier version merged a
# caller-supplied list over a default one with c(), which keeps BOTH
# entries when a name appears twice and silently hands testServer() the
# default rather than the reactiveVal the test is asserting against.
explorer_args <- function(compare_code,
                          selected_code = shiny::reactiveVal(NULL),
                          legality = NULL, traits = NULL) {
  list(
    compare_code = compare_code,
    cards = mini_pool_cardpool(),
    matchup = build_mini_matchup(),
    selected_code = selected_code,
    traits = traits,
    legality = legality
  )
}

test_that("opening an ice lists only the breakers that can break it", {
  # The subtype filter is compute_ice_breaker_matchups()'s job, not this
  # module's, so this asserts the module does not widen it: ice01 is a
  # Barrier, so the two Fracters appear and the Decoder and Killer do
  # not. This is the property that makes the view worth having -- a
  # "one ice vs all breakers" table that listed every breaker would be
  # 173 rows of mostly nonsense.
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code),
    {
      compare_code("ice01")
      session$flushReact()
      rendered <- as.character(output$matchup_table)

      expect_match(rendered, "Bargain Breaker", fixed = TRUE)
      expect_match(rendered, "Fixed Breaker", fixed = TRUE)
      expect_false(grepl("Pricey Breaker", rendered, fixed = TRUE))
      expect_false(grepl("Idle Breaker", rendered, fixed = TRUE))
    }
  )
})

test_that("opening a breaker lists only the ice it can break", {
  # The mirror of the test above, and the reason the mode selector is
  # gone: which of the two questions is being asked follows from the
  # card, so there is nothing left for a radio button to decide.
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code),
    {
      compare_code("brk01")
      session$flushReact()
      rendered <- as.character(output$matchup_table)

      expect_match(rendered, "Cheap Wall", fixed = TRUE)
      expect_match(rendered, "Tall Wall", fixed = TRUE)
      expect_false(grepl("Expensive Code", rendered, fixed = TRUE))
      expect_false(grepl("Untouched Gate", rendered, fixed = TRUE))
    }
  )
})

test_that("the opened card gets no column of its own; only the counterpart does", {
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code),
    {
      compare_code("ice01")
      session$flushReact()
      rendered <- as.character(output$matchup_table)

      expect_match(rendered, "Breaker", fixed = TRUE)
      # The opened ice's own title would be the same value on every row.
      expect_false(grepl("Cheap Wall", rendered, fixed = TRUE))
    }
  )
})

test_that("a not_computable pair renders 'not yet computable', not a blank/NA cell", {
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code),
    {
      # ice03's subroutine count is variable by design and brk03's break
      # cost is not credits, so the pair is honestly unknown.
      compare_code("ice03")
      session$flushReact()
      expect_match(as.character(output$matchup_table), "not yet computable")
    }
  )
})

test_that("clicking a counterpart hands off to the detail modal and clears compare_code", {
  # The two modals share the single '#shiny-modal' element and must not
  # stack, so this transition dismisses one before opening the other.
  selected_code <- shiny::reactiveVal(NULL)
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code, selected_code = selected_code),
    {
      compare_code("ice01")
      session$flushReact()
      session$setInputs(row_card_clicked = "brk01")

      expect_equal(selected_code(), "brk01")
      expect_null(compare_code())
    }
  )
})

test_that("any dismissal clears compare_code via the hidden.bs.modal bridge", {
  # As in test-mod-card-detail.R: testServer() has no DOM, so this
  # simulates the client-side bridge firing rather than the JS itself.
  # Without it, reopening the SAME card after an Escape dismissal would
  # no-op, since the observer only fires on a change.
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code),
    {
      compare_code("ice01")
      session$flushReact()
      session$setInputs(dismissed = 1)
      expect_null(compare_code())
    }
  )
})

test_that("compare_code can be driven through several codes in one session without error", {
  # Same one-instantiation-per-session discipline as
  # mod_card_detail_server(), and the same regression guard: driving both
  # card types through one instance must not error.
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code),
    {
      for (code in c("ice01", "brk01", "ice03", "brk02", "ice01")) {
        compare_code(code)
        session$flushReact()
        expect_no_error(output$matchup_status)
      }
    }
  )
})

test_that("the format filter defaults to Standard and drops pairs it excludes", {
  # brk02 is a Decoder, so its only pair in the mini pool is against
  # ice02 -- which Standard bans. The pair therefore exists but is not
  # legal, which is a different answer from "this breaker has no
  # matchups", and the empty state has to say which.
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code,
                         legality = matchup_legality_fixture()),
    {
      compare_code("brk02")
      session$setInputs(format = "standard")
      session$flushReact()

      expect_match(as.character(output$matchup_status$html), "No matchups in this format")
    }
  )
})

test_that("'Any format' restores the pairs the format filter removed", {
  # The matchup table itself is format-blind, so "Any format" is a real
  # choice and not a placeholder -- the same contract mod_card_browser
  # offers.
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code,
                         legality = matchup_legality_fixture()),
    {
      compare_code("brk02")
      session$setInputs(format = "")
      session$flushReact()

      expect_match(as.character(output$matchup_table), "Expensive Code", fixed = TRUE)
    }
  )
})

test_that("both sides of a pair must be format-legal, not just the opened card", {
  # ice01 is Standard-legal and so are both Fracters, so this is the
  # positive control for the test above: a legal pair survives the same
  # filter that removed the banned one.
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code,
                         legality = matchup_legality_fixture()),
    {
      compare_code("ice01")
      session$setInputs(format = "standard")
      session$flushReact()

      expect_match(as.character(output$matchup_table), "Bargain Breaker", fixed = TRUE)
    }
  )
})

test_that("NULL legality disables the format filter rather than silently applying one", {
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code, legality = NULL),
    {
      compare_code("brk02")
      session$flushReact()
      # ice02 is banned in the fixture above, but with no legality data
      # there is nothing to ban it with.
      expect_match(as.character(output$matchup_table), "Expensive Code", fixed = TRUE)
    }
  )
})

test_that("empty_matchup_reason names the format when pairs exist but none are legal", {
  cards <- mini_pool_cardpool()
  pairs <- build_mini_matchup()
  brk02_pairs <- pairs[pairs$breaker_code == "brk02", , drop = FALSE]

  expect_match(
    empty_matchup_reason(cards[cards$code == "brk02", ], brk02_pairs),
    "format"
  )
})

test_that("empty_matchup_reason blames our parser, not the card, for an unreadable break clause", {
  # 36 of the 173 icebreakers in the real pool have an unparsed
  # break_subtype, and breaker_matches_ice() treats that as FALSE. A bare
  # "no matchups" would report that gap as a property of the card, which
  # is the kind of plausible-looking wrong answer this codebase does not
  # ship.
  cards <- mini_pool_cardpool()
  traits <- mini_pool_ice_breaker_traits()
  traits$break_subtype[traits$code == "brk01"] <- NA_character_

  msg <- empty_matchup_reason(
    cards[cards$code == "brk01", ], build_mini_matchup()[0, ], traits
  )

  expect_match(msg, "could not be read")
  expect_match(msg, "not a claim that it breaks nothing")
})

test_that("empty_matchup_reason stays generic when traits are unavailable to explain", {
  # Without traits there is no evidence for the parser-gap message, so it
  # is not asserted.
  cards <- mini_pool_cardpool()

  expect_equal(
    empty_matchup_reason(cards[cards$code == "brk01", ], build_mini_matchup()[0, ], NULL),
    "No matchups for this card."
  )
})

test_that("a not_computable cost renders an em dash, not an empty cell", {
  # reactable prints NA as a zero-width space, which looks identical to a
  # value nobody got round to filling in. not_computable is a state this
  # app states outright, so the cell has to show something.
  compare_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = explorer_args(compare_code = compare_code),
    {
      compare_code("ice03")
      session$flushReact()
      expect_match(as.character(output$matchup_table), "\u2014")
    }
  )
})
