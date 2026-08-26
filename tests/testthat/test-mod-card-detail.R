test_that("mod_card_detail_server renders the selected card's detail", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

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
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(mod_card_detail_server, args = list(selected_code = selected_code, cards = cards), {
    for (code in c("ice01", "brk01", "ice02", "brk02", "ice01")) {
      selected_code(code)
      session$flushReact()
      expect_no_error(output$detail)
    }
  })
})

test_that("the Close button does not error (removeModal() has no server-testable effect)", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(mod_card_detail_server, args = list(selected_code = selected_code, cards = cards), {
    selected_code("ice01")
    session$flushReact()
    expect_no_error(session$setInputs(close = 1))
  })
})

test_that("any dismissal (backdrop click, Escape, or Close) clears selected_code() via the hidden.bs.modal bridge", {
  # The actual dismissal signal is client-side JS (Bootstrap's
  # hidden.bs.modal event, wired in mod_card_detail_server's showModal()
  # call) setting input$dismissed -- shiny::testServer() has no real DOM,
  # so this simulates that bridge firing rather than exercising the JS
  # itself. This is what replaced easyClose = FALSE: a real
  # click-outside/Escape dismissal now clears selected_code() too, not
  # just the Close button.
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(mod_card_detail_server, args = list(selected_code = selected_code, cards = cards), {
    selected_code("ice01")
    session$flushReact()
    session$setInputs(dismissed = 1)
    expect_null(selected_code())
  })
})
