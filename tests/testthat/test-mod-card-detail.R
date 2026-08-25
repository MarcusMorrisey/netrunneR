test_that("mod_card_detail_server renders the selected card's detail", {
  cards <- mini_pool_cardpool()
  selected_code <- reactiveVal(NULL)

  shiny::testServer(mod_card_detail_server, args = list(selected_code = selected_code, cards = cards), {
    selected_code("ice01")
    session$flushReact()
    expect_match(as.character(output$detail), "Cheap Wall")
  })
})

test_that("selected_code can be driven through several codes in one session without error", {
  # Regression guard: mod_card_detail_server is instantiated ONCE by
  # testServer() here, matching app_server()'s one-instantiation-per-
  # session discipline -- it must not error or need re-instantiation per
  # code, unlike an earlier draft that called moduleServer() fresh per
  # click and leaked a growing set of observers.
  cards <- mini_pool_cardpool()
  selected_code <- reactiveVal(NULL)

  shiny::testServer(mod_card_detail_server, args = list(selected_code = selected_code, cards = cards), {
    for (code in c("ice01", "brk01", "ice02", "brk02", "ice01")) {
      selected_code(code)
      session$flushReact()
      expect_no_error(output$detail)
    }
  })
})

test_that("the Close button clears selected_code()", {
  cards <- mini_pool_cardpool()
  selected_code <- reactiveVal(NULL)

  shiny::testServer(mod_card_detail_server, args = list(selected_code = selected_code, cards = cards), {
    selected_code("ice01")
    session$flushReact()
    session$setInputs(close = 1)
    expect_null(selected_code())
  })
})
