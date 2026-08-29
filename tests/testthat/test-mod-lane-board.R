build_mini_matchup <- function() {
  compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )$matchups
}

# The board's whole output is one renderUI, so every assertion below reads
# the same rendered string. Rendering it once, here, keeps each test about
# what it is actually checking.
render_board <- function(add_ice = character(0), lane_breakers = list(),
                         selected_code = shiny::reactiveVal(NULL)) {
  cards <- mini_pool_cardpool()
  matchup <- build_mini_matchup()
  out <- NULL
  shiny::testServer(
    mod_lane_board_server,
    args = list(cards = cards, matchup = matchup, selected_code = selected_code),
    {
      board <- session$getReturned()
      for (code in add_ice) board$add_ice(code)
      # `lane_breakers` is a named list, ice code -> breakers for THAT
      # lane. The name avoids `breakers`, which is a reactiveVal inside
      # the module -- testServer() evaluates this block in the module's
      # environment, so a matching name resolves to the module's copy and
      # the argument is never seen.
      # Pressing a lane's "+" is what tells the board which lane a pick
      # belongs to, so the test presses it rather than reaching past it.
      for (ice_code in names(lane_breakers)) {
        for (bc in lane_breakers[[ice_code]]) {
          session$setInputs(add_breaker = ice_code)
          board$add_breaker(bc)
        }
      }
      session$flushReact()
      # renderUI's value in testServer is the list shiny sends to the
      # client (html + dependencies), not a string -- the markup is the
      # $html element.
      out <<- output$lanes$html
    }
  )
  out
}

test_that("an empty board invites an ice rather than rendering nothing", {
  rendered <- render_board()
  expect_match(rendered, "ADD ICE TO COMPARE")
})

test_that("an added ice anchors a lane, named but not written over", {
  rendered <- render_board(add_ice = "ice01")
  # The card image carries the publisher's own title, type line, cost and
  # strength. The name is still reachable -- as the accessible name and
  # the hover -- but nothing is painted across the art to repeat it.
  expect_match(rendered, 'title="Cheap Wall"')
  expect_match(rendered, 'alt="Cheap Wall"')
  expect_no_match(rendered, "nr-lane-label")
  expect_no_match(rendered, "nr-pip")
  expect_no_match(rendered, "STR 1")
})

test_that("a hand-curated pair is badged OVERRIDE, not passed off as derived", {
  # The distinction is the point of the badge: an override is somebody's
  # judgement, and a reader who cannot tell it from a computed value has
  # been misled about how much to trust it.
  rendered <- render_board(add_ice = "ice01", lane_breakers = list(ice01 = "brk01"))
  expect_match(rendered, "OVERRIDE")
  expect_no_match(rendered, "FORMULA")
})

test_that("a runner-favored differential reads as positive and a corp-favored one as negative", {
  runner_favored <- render_board(add_ice = "ice01", lane_breakers = list(ice01 = "brk01"))
  expect_match(runner_favored, "nr-diff-good")
  expect_match(runner_favored, "[+]1")

  corp_favored <- render_board(add_ice = "ice02", lane_breakers = list(ice02 = "brk02"))
  expect_match(corp_favored, "nr-diff-bad")
  # Rendered as a real minus sign (U+2212), not a hyphen.
  expect_match(corp_favored, "\u22122")
})

test_that("an uncomputable pair says so instead of showing a blank strip", {
  # A blank would read as "zero cost to break", which is the opposite of
  # what compute_cost_to_break_formula() is admitting for this pair.
  rendered <- render_board(add_ice = "ice03", lane_breakers = list(ice03 = "brk03"))
  expect_match(rendered, "NOT COMPUTABLE")
  expect_no_match(rendered, "nr-diff-good")
  expect_no_match(rendered, "nr-diff-bad")
})

test_that("a pair with no matchup row at all is uncomputable, not silently blank", {
  # Same statement as not_computable: an absent row and an uncomputable
  # one are equally "we cannot tell you". stat_strip_ui() is called
  # directly with an empty frame because compute_ice_breaker_matchups()
  # emits a row for every pair, so the state is unreachable through it --
  # which is exactly why it needs pinning rather than assuming.
  rendered <- as.character(stat_strip_ui(build_mini_matchup()[0, ]))
  expect_match(rendered, "NOT COMPUTABLE")
})

test_that("a breaker added to one lane does not appear in the others", {
  # The lane's "+" adds to that lane. An earlier draft shared one breaker
  # set across every lane, which made a single "+" silently change every
  # column.
  rendered <- render_board(add_ice = c("ice01", "ice02"),
                           lane_breakers = list(ice01 = "brk01"))
  # Twice for the one card that is present -- title= and alt= -- and not
  # at all for the lane it was never added to.
  expect_equal(length(gregexpr("Bargain Breaker", rendered)[[1]]), 2L)
})

test_that("each lane carries its own breakers and its own strip", {
  rendered <- render_board(add_ice = c("ice01", "ice02"),
                           lane_breakers = list(ice01 = "brk01", ice02 = "brk02"))
  expect_match(rendered, "Bargain Breaker")
  expect_match(rendered, "Pricey Breaker")
  # ice01/brk01 is runner-favored and ice02/brk02 corp-favored, so both
  # signs are present -- the strips belong to their own pairings, not to
  # a set shared down the board.
  expect_match(rendered, "nr-diff-good")
  expect_match(rendered, "nr-diff-bad")
})

test_that("adding the same ice twice does not duplicate its lane", {
  rendered <- render_board(add_ice = c("ice01", "ice01"))
  expect_equal(length(gregexpr("Cheap Wall", rendered)[[1]]), 2L)
})

test_that("clicking a card sets the shared selected_code", {
  cards <- mini_pool_cardpool()
  matchup <- build_mini_matchup()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_lane_board_server,
    args = list(cards = cards, matchup = matchup, selected_code = selected_code),
    {
      session$setInputs(card_clicked = "ice01")
      expect_equal(selected_code(), "ice01")
    }
  )
})

test_that("LOAD DECK ships visibly disabled rather than quietly absent", {
  # The wireframe specifies it; the nrdb mirror deliberately carries no
  # decklists. Dropping the control would lose the design intent, and
  # wiring it would promise data that is not there -- so it renders dead,
  # and this pins that rather than letting a later tidy-up delete it.
  rendered <- as.character(mod_lane_board_ui("board"))
  expect_match(rendered, "LOAD DECK")
  expect_match(rendered, "disabled")
})

test_that("sibling apps that do not exist are inert, not links", {
  rendered <- as.character(suite_nav_ui("iceBreaker"))
  expect_match(rendered, "Meta Maps")
  expect_no_match(rendered, "<a[^>]*Meta Maps")
})

test_that("every lane gets its own add-breaker slot, carrying its own ice code", {
  rendered <- render_board(add_ice = c("ice01", "ice02", "ice03"))
  expect_equal(length(gregexpr("nr-slot-portrait", rendered)[[1]]), 3L)
  # Each slot names its lane, which is how a pick knows where to land.
  for (code in c("ice01", "ice02", "ice03")) {
    expect_match(rendered, sprintf("add_breaker&#39;, &#39;%s&#39;", code))
  }
})

test_that("a picked breaker with no lane pending is dropped, not guessed at", {
  cards <- mini_pool_cardpool()
  matchup <- build_mini_matchup()
  shiny::testServer(
    mod_lane_board_server,
    args = list(cards = cards, matchup = matchup,
                selected_code = shiny::reactiveVal(NULL)),
    {
      board <- session$getReturned()
      board$add_ice("ice01")
      board$add_breaker("brk01")   # no "+" pressed first
      session$flushReact()
      expect_no_match(output$lanes$html, "Bargain Breaker")
    }
  )
})
