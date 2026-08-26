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
