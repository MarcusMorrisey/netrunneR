build_mini_matchup <- function() {
  compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )$matchups
}

test_that("a not_computable pair renders 'not yet computable', not a blank/NA cell", {
  cards <- mini_pool_cardpool()
  matchup <- build_mini_matchup()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = list(cards = cards, matchup = matchup, selected_code = selected_code),
    {
      session$setInputs(mode = "all", breaker_code = "brk03", ice_code = "ice03")
      session$flushReact()
      rendered <- as.character(output$matchup_table)
      expect_match(rendered, "not yet computable")
    }
  )
})

test_that("row click on ice/breaker name sets the shared selected_code", {
  cards <- mini_pool_cardpool()
  matchup <- build_mini_matchup()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = list(cards = cards, matchup = matchup, selected_code = selected_code),
    {
      session$setInputs(mode = "all", breaker_code = "brk01", ice_code = "ice01")
      session$setInputs(row_card_clicked = "ice01")
      expect_equal(selected_code(), "ice01")
    }
  )
})

test_that("an empty result set renders an explicit empty state", {
  cards <- mini_pool_cardpool()
  matchup <- build_mini_matchup()[0, ]
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_matchup_explorer_server,
    args = list(cards = cards, matchup = matchup, selected_code = selected_code),
    {
      session$setInputs(mode = "all", breaker_code = "brk01", ice_code = "ice01")
      session$flushReact()
      # The empty-state message lives in its own renderUI() output, not
      # inside the reactable slot -- reactableOutput() is htmlwidget-typed
      # and can't display an arbitrary shiny.tag, only a reactable widget
      # (or nothing, via NULL).
      rendered <- as.character(output$matchup_status$html)
      expect_match(rendered, "No matchups")
    }
  )
})
