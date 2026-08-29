test_that("a filter combination matching zero cards renders an explicit empty state", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      # No corp card is an Icebreaker -- the same non-overlap that made
      # the Type picker redundant is what makes this combination empty.
      session$setInputs(side = "corp", faction = character(0), subtype = "Icebreaker")
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
      session$setInputs(subtype = "Icebreaker")
      session$flushReact()
      narrowed <- as.character(output$card_grid$html)
      expect_false(grepl("ice01", narrowed, fixed = TRUE))

      session$setInputs(subtype = character(0))
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

test_that("Clear all filters resets every filter, including format", {
  # Nothing asserted that the observer actually fires -- the neighbouring
  # test only covers the downstream half of the contract (an empty
  # selection reads as "filter off"). The button was verified by hand in
  # a running app instead, which does not survive a refactor.
  #
  # Format must reset to "" (Any format), not to Standard: a "clear all
  # filters" that leaves a format applied has not cleared all filters.
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)
  updates <- new.env(parent = emptyenv())
  updates$calls <- list()

  record <- function(session, inputId, ...) {
    args <- list(...)
    value <- if ("value" %in% names(args)) args$value else args$selected
    updates$calls[[inputId]] <- value
    invisible(NULL)
  }

  testthat::local_mocked_bindings(updateTextInput = record, .package = "shiny")
  testthat::local_mocked_bindings(updatePickerInput = record, .package = "shinyWidgets")
  # Side is a radio now, so it resets through a different function; a
  # mock covering only the pickers would silently stop watching it.
  testthat::local_mocked_bindings(updateRadioGroupButtons = record, .package = "shinyWidgets")

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(clear = 1)
      session$flushReact()
    }
  )

  # No "type": that control is gone in this version, and a stale name
  # here would be the test still believing in a filter the UI dropped.
  expect_setequal(
    names(updates$calls),
    c("query", "format", "side", "faction", "subtype")
  )
  expect_identical(updates$calls$query, "")
  expect_identical(updates$calls$format, "")
  # Any, not the Corp default: clearing to a default would leave a side
  # applied, which is the same mistake as clearing format back to
  # Standard.
  expect_identical(updates$calls$side, "")
  expect_identical(updates$calls$subtype, character(0))
})

test_that("Side is exclusive: one side at a time, and Any means both", {
  # Only Corps have ICE and only Runners have programs, so the two pools
  # do not overlap -- holding both open at once buys nothing, and the
  # control is a radio rather than a multi-select for that reason.
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(side = "corp")
      session$flushReact()
      corp_only <- as.character(output$card_grid$html)
      expect_match(corp_only, "ice01", fixed = TRUE)
      expect_false(grepl("brk01", corp_only, fixed = TRUE))

      # Choosing the other side replaces rather than adds to it.
      session$setInputs(side = "runner")
      session$flushReact()
      runner_only <- as.character(output$card_grid$html)
      expect_match(runner_only, "brk01", fixed = TRUE)
      expect_false(grepl("ice01", runner_only, fixed = TRUE))

      # "" is the Any state. It is a value, not an absent input, which is
      # why the filter tests nzchar() rather than length().
      session$setInputs(side = "")
      session$flushReact()
      both <- as.character(output$card_grid$html)
      expect_match(both, "ice01", fixed = TRUE)
      expect_match(both, "brk01", fixed = TRUE)
    }
  )
})

test_that("filters are cumulative across controls", {
  # Each control narrows what the last one left; they are ANDed, not
  # ORed. Runner AND Icebreaker is the runner breakers, not every runner
  # card plus every icebreaker.
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(side = "runner", subtype = "Icebreaker")
      session$flushReact()
      html <- as.character(output$card_grid$html)
      expect_match(html, "brk01", fixed = TRUE)
      expect_false(grepl("ice01", html, fixed = TRUE))

      # A combination whose halves each match something, but whose
      # intersection is empty, must render empty rather than either half.
      session$setInputs(side = "corp", subtype = "Icebreaker")
      session$flushReact()
      expect_match(as.character(output$card_grid$html), "No cards match")
    }
  )
})

test_that("the Subtype picker matches whole tokens, like the query box does", {
  # keywords is " - "-delimited, so a substring match pulls in any
  # subtype that merely CONTAINS the selected one. Against the live pool
  # that was two real collisions: picking "Corp" also returned
  # Corporation cards, picking "Security" also returned Security
  # Protocol. The query box's `s:Corp` was already correct, so the two
  # paths disagreed about the same question.
  cards <- tibble::tribble(
    ~code,   ~title,      ~pack_code, ~faction_code,  ~type_code, ~side_code, ~text, ~cost, ~strength, ~keywords,
    "has01", "Real Corp", "core",     "neutral-corp", "ice",      "corp",     "",    2L,    1L,        "Corp",
    "sub01", "Not A Corp","core",     "neutral-corp", "ice",      "corp",     "",    2L,    1L,        "Corporation",
    "mid01", "Middle",    "core",     "neutral-corp", "ice",      "corp",     "",    2L,    1L,        "Barrier - Corp - AP"
  )
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(subtype = "Corp")
      session$flushReact()
      html <- as.character(output$card_grid$html)

      expect_match(html, "has01", fixed = TRUE)
      # A token in the middle of the list still counts.
      expect_match(html, "mid01", fixed = TRUE)
      # The whole point: Corporation is not Corp.
      expect_false(grepl("sub01", html, fixed = TRUE))
    }
  )
})

test_that("selecting several subtypes is an OR, and none is no filter", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(side = "", subtype = c("Barrier", "Icebreaker"))
      session$flushReact()
      either <- as.character(output$card_grid$html)
      expect_match(either, "ice01", fixed = TRUE)   # Barrier
      expect_match(either, "brk01", fixed = TRUE)   # Icebreaker
      expect_false(grepl("ice02", either, fixed = TRUE))  # Code Gate, neither

      session$setInputs(subtype = character(0))
      session$flushReact()
      expect_match(as.character(output$card_grid$html), "ice02", fixed = TRUE)
    }
  )
})

test_that("a subtype containing a regex metacharacter is matched literally", {
  # No current subtype carries one, so this is a guard against upstream
  # rather than a live bug -- the picker's choices come from mirrored
  # data, and the old code interpolated them straight into a pattern.
  cards <- tibble::tribble(
    ~code,   ~title,   ~pack_code, ~faction_code,  ~type_code, ~side_code, ~text, ~cost, ~strength, ~keywords,
    "meta01","Literal","core",     "neutral-corp", "ice",      "corp",     "",    1L,    1L,        "G.mod",
    "meta02","Decoy",  "core",     "neutral-corp", "ice",      "corp",     "",    1L,    1L,        "GXmod"
  )
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$setInputs(subtype = "G.mod")
      session$flushReact()
      html <- as.character(output$card_grid$html)
      expect_match(html, "meta01", fixed = TRUE)
      # "." must not have matched the X.
      expect_false(grepl("meta02", html, fixed = TRUE))
    }
  )
})

test_that("a picker mounted over one side's pool renders no Side control", {
  # app_server() mounts this module twice as an add-card picker, each over
  # a pool already narrowed to one side. There the control has no
  # non-destructive state to offer: it can restate the choice already made
  # or empty the grid. filtered() reads input$side %||% "", so omitting it
  # is already the Any state.
  picker <- as.character(mod_card_browser_ui("b", side = NULL))
  expect_no_match(picker, "Side")

  browsing <- as.character(mod_card_browser_ui("b"))
  expect_match(browsing, "Side")
  expect_match(browsing, 'value="corp"[^>]*checked')

  expect_error(mod_card_browser_ui("b", side = "nonsense"))
})
