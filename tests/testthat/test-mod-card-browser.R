test_that("a filter combination matching zero cards renders an explicit empty state", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(side = "corp", faction = character(0), type = "program", subtype = character(0))
      session$flushReact()
      rendered <- as.character(output$card_grid$html)
      expect_match(rendered, "No cards match")
    }
  )
})

test_that("clicking a card sets the shared selected_code", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(card_clicked = "ice01")
      expect_equal(selected_code(), "ice01")
    }
  )
})

# ---- search + legality wiring ------------------------------------------

test_that("browser_search_fields() merges the card and legality registries", {
  reg <- browser_search_fields()
  # Both halves resolve, canonical names and aliases alike. The merge is
  # the point: search_field_registry() aborts on a duplicate alias, so
  # this also pins that the two field sets stay collision-free.
  expect_true(all(c("title", "t", "cost", "s", "faction") %in% names(reg)))
  expect_true(all(c("is_banned", "banned", "is_restricted", "points") %in% names(reg)))
  expect_identical(attr(reg, "default_field"), "title")
})

# A legality bundle shaped like load_ice_breaker_app_data()$legality,
# covering the mini pool: every card is in the Standard pool except
# ice03, which sits in a set no pool lists, and ice02 is banned.
browser_legality_fixture <- function() {
  list(
    format = tibble::tribble(
      ~id,        ~name,
      "standard", "Standard",
      "startup",  "Startup"
    ),
    format_snapshot = tibble::tribble(
      ~id,          ~format_id,  ~date_start,  ~card_pool_id, ~restriction_id, ~is_active,
      "standard_1", "standard",  "2024-01-01", "pool_std",    "ban_std",       1L,
      "startup_1",  "startup",   "2024-01-01", "pool_start",  NA_character_,   1L
    ),
    card_pool_set = tibble::tribble(
      ~card_pool_id, ~card_set_id,
      "pool_std",    "set_core",
      "pool_start",  "set_core"
    ),
    restriction_card = tibble::tribble(
      ~restriction_id, ~card_id,  ~is_banned, ~is_restricted, ~universal_faction_cost, ~global_penalty, ~points,
      "ban_std",       "ice02",   1L,         NA_integer_,    NA_integer_,             NA_integer_,     NA_integer_
    ),
    printing = tibble::tribble(
      ~code,   ~card_id, ~card_set_id,
      "ice01", "ice01",  "set_core",
      "ice02", "ice02",  "set_core",
      "ice03", "ice03",  "set_outside",
      "brk01", "brk01",  "set_core",
      "brk02", "brk02",  "set_core",
      "brk03", "brk03",  "set_core"
    ),
    card_set = tibble::tribble(
      ~id,           ~legacy_code,
      "set_core",    "core",
      "set_outside", "outside"
    )
  )
}

test_that("a search query filters the grid", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(query = "t:ice")
      session$flushReact()
      rendered <- as.character(output$card_grid$html)
      expect_match(rendered, "ice01", fixed = TRUE)
      expect_false(grepl("brk01", rendered, fixed = TRUE))
    }
  )
})

test_that("a malformed query reports itself and leaves the grid unfiltered", {
  # Silently emptying the grid would read as "no cards match" rather than
  # "that query is malformed" -- the failure mode search_explain() and
  # the classed conditions exist to prevent.
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(query = "nosuchfield:xyz")
      session$flushReact()
      expect_match(as.character(output$query_feedback$html), "nosuchfield")
      expect_match(as.character(output$card_grid$html), "ice01", fixed = TRUE)
    }
  )
})

test_that("a valid query is echoed back in words", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(query = "t:ice")
      session$flushReact()
      expect_match(as.character(output$query_feedback$html), "Reading as")
    }
  )
})

test_that("a chosen format splits out-of-pool cards into the collapsed group", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server,
    args = list(cards = cards, selected_code = selected_code,
                legality = browser_legality_fixture()),
    {
      session$setInputs(format = "standard")
      session$flushReact()
      rendered <- as.character(output$card_grid$html)
      # ice03 is in no pool, so it is hidden rather than gone.
      expect_match(rendered, "hidden by card pool")
      expect_match(rendered, "ice03", fixed = TRUE)
    }
  )
})

test_that("a banned card is dimmed rather than dropped", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server,
    args = list(cards = cards, selected_code = selected_code,
                legality = browser_legality_fixture()),
    {
      session$setInputs(format = "standard")
      session$flushReact()
      rendered <- as.character(output$card_grid$html)
      expect_match(rendered, "ice02", fixed = TRUE)
      expect_match(rendered, "Banned in this format", fixed = TRUE)
    }
  )
})

test_that("legality columns are searchable in the browser's own query box", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server,
    args = list(cards = cards, selected_code = selected_code,
                legality = browser_legality_fixture()),
    {
      session$setInputs(format = "standard", query = "banned:true")
      session$flushReact()
      rendered <- as.character(output$card_grid$html)
      expect_match(rendered, "ice02", fixed = TRUE)
      expect_false(grepl("brk01", rendered, fixed = TRUE))
    }
  )
})

test_that("no legality data leaves every card in pool and no format applied", {
  # The state of any release promoted before the format schema existed.
  # It must render the pool unfiltered rather than fail, and must not
  # imply a format is in force.
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$flushReact()
      rendered <- as.character(output$card_grid$html)
      expect_false(grepl("hidden by card pool", rendered))
      expect_match(rendered, "ice01", fixed = TRUE)
      expect_match(rendered, "brk03", fixed = TRUE)
    }
  )
})

test_that("emptying a filter restores the full grid rather than matching nothing", {
  # The contract "Clear all filters" depends on: updatePickerInput()
  # sets each picker to character(0), and length(input$x) == 0 must read
  # as "this filter is off". If an empty selection filtered to zero rows
  # instead, clearing would blank the browser.
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(type = "program")
      session$flushReact()
      narrowed <- as.character(output$card_grid$html)
      expect_false(grepl("ice01", narrowed, fixed = TRUE))

      session$setInputs(type = character(0))
      session$flushReact()
      restored <- as.character(output$card_grid$html)
      expect_match(restored, "ice01", fixed = TRUE)
      expect_match(restored, "brk01", fixed = TRUE)
    }
  )
})

test_that("an empty query is no filter, not a failed parse", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(query = "t:ice")
      session$flushReact()
      expect_false(grepl("brk01", as.character(output$card_grid$html), fixed = TRUE))

      session$setInputs(query = "")
      session$flushReact()
      restored <- as.character(output$card_grid$html)
      expect_match(restored, "brk01", fixed = TRUE)
      # And no leftover "Reading as:" line describing a query that is
      # gone. Compared as text rather than expect_null(): renderUI()
      # returning NULL can surface as NULL or as an empty list, and the
      # assertion is about what the user sees either way.
      feedback <- paste(as.character(output$query_feedback$html), collapse = "")
      expect_false(grepl("Reading as", feedback, fixed = TRUE))
    }
  )
})
