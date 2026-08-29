build_mini_matchup <- function() {
  compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )$matchups
}

# The board's whole output is one renderUI, so every assertion below reads
# the same rendered string. Rendering it once, here, keeps each test about
# what it is actually checking.
render_board <- function(add_ice = character(0), lane_breakers = list(),
                         selected_code = shiny::reactiveVal(NULL),
                         traits = NULL, override = character(0)) {
  cards <- mini_pool_cardpool()
  matchup <- build_mini_matchup()
  out <- NULL
  shiny::testServer(
    mod_lane_board_server,
    args = list(cards = cards, matchup = matchup, selected_code = selected_code,
                traits = traits),
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
      # Ticking the "compute anyway" box, through the same input the
      # checkbox sets rather than by reaching into the reactiveVal.
      for (key in override) session$setInputs(override_toggled = key)
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

test_that("an absent row with no traits to explain it stays uncomputable", {
  # Without `traits` there is nothing to tell "the filter dropped this on
  # purpose" from "we could not read the breaker", so both collapse to the
  # weaker statement. That is what this module said before it could tell
  # them apart: less than we know now, but not wrong.
  ice <- mini_pool_cardpool()[mini_pool_cardpool()$code == "ice02", ]
  brk <- mini_pool_cardpool()[mini_pool_cardpool()$code == "brk01", ]
  state <- matchup_pair_state(ice, brk, build_mini_matchup()[0, ], traits = NULL)

  expect_equal(state$kind, "unknown")
  expect_match(as.character(stat_strip_ui(state)), "NOT COMPUTABLE")
})

test_that("a Fracter stacked under a Code Gate says it CANNOT break it", {
  # The case that prompted this: a Fracter under a Code Gate produced no
  # matchup row, and an absent row rendered as "not computable" -- which
  # says we do not know, when in fact this is the most definite statement
  # the app can make about a pairing.
  ice <- mini_pool_cardpool()[mini_pool_cardpool()$code == "ice02", ]   # Code Gate
  brk <- mini_pool_cardpool()[mini_pool_cardpool()$code == "brk01", ]   # Fracter
  state <- matchup_pair_state(ice, brk, build_mini_matchup()[0, ],
                              mini_pool_ice_breaker_traits())

  expect_equal(state$kind, "cannot_break")
  expect_true(state$overridable)
  rendered <- as.character(stat_strip_ui(state))
  expect_match(rendered, "CANNOT BREAK")
  expect_no_match(rendered, "NOT COMPUTABLE")
})

test_that("a breaker whose break clause could not be read is NOT called incompatible", {
  # An unreadable break clause means we do not know what it breaks, so we
  # are in no position to say it cannot break this. Reporting our own
  # parser gap as a fact about the card is the failure mode this whole
  # table exists to avoid.
  traits <- mini_pool_ice_breaker_traits()
  traits$break_subtype[traits$code == "brk01"] <- NA_character_

  ice <- mini_pool_cardpool()[mini_pool_cardpool()$code == "ice02", ]
  brk <- mini_pool_cardpool()[mini_pool_cardpool()$code == "brk01", ]
  state <- matchup_pair_state(ice, brk, build_mini_matchup()[0, ], traits)

  expect_equal(state$kind, "unknown")
  expect_false(state$overridable)
  expect_match(as.character(stat_strip_ui(state)), "NOT COMPUTABLE")
})

test_that("the incompatible pair offers a way to override it", {
  rendered <- render_board(
    add_ice = "ice02", lane_breakers = list(ice02 = "brk01"),
    traits = mini_pool_ice_breaker_traits()
  )

  expect_match(rendered, "CANNOT BREAK")
  expect_match(rendered, "nr-override")
  expect_match(rendered, "compute anyway")
})

test_that("overriding an incompatible pair prices it and badges it ASSUMED", {
  # brk01 strength 1 vs ice02 strength 3: gap 2 at +1 a pump = 2 credits,
  # plus 2 subroutines at 1 each = 2. Total 4, against a rez of 8, so the
  # differential is +4. The arithmetic is ordinary; only the premise that
  # a Fracter may break a Code Gate at all is the operator's.
  rendered <- render_board(
    add_ice = "ice02", lane_breakers = list(ice02 = "brk01"),
    traits = mini_pool_ice_breaker_traits(),
    override = "ice02|brk01"
  )

  expect_match(rendered, "ASSUMED")
  expect_no_match(rendered, "CANNOT BREAK")
  expect_match(rendered, "nr-diff-good")
  expect_match(rendered, "[+]4")
})

test_that("an override is never badged FORMULA", {
  # The number is real; the premise is not ours. A reader must be able to
  # tell this from a pairing the subtypes actually allow.
  rendered <- render_board(
    add_ice = "ice02", lane_breakers = list(ice02 = "brk01"),
    traits = mini_pool_ice_breaker_traits(),
    override = "ice02|brk01"
  )

  expect_no_match(rendered, "nr-badge-formula")
})

test_that("the override toggles back off", {
  # An assertion you cannot withdraw is a trap. Setting the same input
  # twice is what a second click on the checkbox does.
  rendered <- render_board(
    add_ice = "ice02", lane_breakers = list(ice02 = "brk01"),
    traits = mini_pool_ice_breaker_traits(),
    override = c("ice02|brk01", "ice02|brk01")
  )

  expect_match(rendered, "CANNOT BREAK")
  expect_no_match(rendered, "ASSUMED")
})

test_that("an override names one lane, not a breaker everywhere", {
  # The same breaker can sit under several ice, and asserting something
  # about one board position says nothing about the others -- the same
  # reasoning remove_breaker's composite key already encodes.
  rendered <- render_board(
    add_ice = c("ice02", "ice03"),
    lane_breakers = list(ice02 = "brk01", ice03 = "brk01"),
    traits = mini_pool_ice_breaker_traits(),
    override = "ice02|brk01"
  )

  expect_match(rendered, "ASSUMED")
  # The other lane's identical pairing is untouched.
  expect_match(rendered, "CANNOT BREAK")
})

test_that("a pairing the subtypes allow offers no override control", {
  # There is nothing to assert about a pair that already computes, and a
  # checkbox there would imply the result is in doubt.
  # ice04 is a Barrier and brk01 is a Fracter, so the subtypes agree and
  # there is nothing to assert. (ice01/brk01 would do just as well for the
  # compatibility, but it carries a curated override in the fixture and
  # would badge OVERRIDE, which is a different story.)
  rendered <- render_board(
    add_ice = "ice04", lane_breakers = list(ice04 = "brk01"),
    traits = mini_pool_ice_breaker_traits()
  )

  expect_match(rendered, "FORMULA")
  expect_no_match(rendered, "CANNOT BREAK")
  expect_no_match(rendered, "nr-override")
})

test_that("the override control does not also open the card detail modal", {
  # The strip sits under a card whose own onclick opens the detail modal,
  # so without stopPropagation a tick would do two things, one of them
  # unasked for. Same reason remove_button() carries it.
  ice <- mini_pool_cardpool()[mini_pool_cardpool()$code == "ice02", ]
  brk <- mini_pool_cardpool()[mini_pool_cardpool()$code == "brk01", ]
  state <- matchup_pair_state(ice, brk, build_mini_matchup()[0, ],
                              mini_pool_ice_breaker_traits())
  session <- shiny::MockShinySession$new()

  expect_match(as.character(override_control_ui(session, state)),
               "event.stopPropagation()", fixed = TRUE)
})

test_that("a breaker added to one lane does not appear in the others", {
  # The lane's "+" adds to that lane. An earlier draft shared one breaker
  # set across every lane, which made a single "+" silently change every
  # column.
  rendered <- render_board(add_ice = c("ice01", "ice02"),
                           lane_breakers = list(ice01 = "brk01"))
  # Counts the rendered card, not mentions of its name: the name also
  # appears in the remove control's label, so a raw string count measures
  # the markup rather than the board.
  expect_equal(length(gregexpr('alt="Bargain Breaker"', rendered, fixed = TRUE)[[1]]), 1L)
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
  expect_equal(length(gregexpr('alt="Cheap Wall"', rendered, fixed = TRUE)[[1]]), 1L)
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

test_that("a breaker known to declare more subtypes than we stored never says CANNOT BREAK", {
  # Penrose and Lobisomem each break two subtypes and this table records
  # one, so the dropped subtype's pairings are absent -- and an absent
  # pairing is what renders as a definite negative. Claiming Penrose
  # cannot break a Code Gate is false for 133 pairings. Partial evidence
  # cannot support a definite negative, so it falls back to "we do not
  # know".
  traits <- mini_pool_ice_breaker_traits()
  traits$break_subtype_count <- 1L
  traits$break_subtype_count[traits$code == "brk01"] <- 2L

  ice <- mini_pool_cardpool()[mini_pool_cardpool()$code == "ice02", ]   # Code Gate
  brk <- mini_pool_cardpool()[mini_pool_cardpool()$code == "brk01", ]   # records Barrier
  state <- matchup_pair_state(ice, brk, build_mini_matchup()[0, ], traits)

  expect_equal(state$kind, "unknown")
  expect_false(state$overridable)
  rendered <- as.character(stat_strip_ui(state))
  expect_match(rendered, "NOT COMPUTABLE")
  expect_no_match(rendered, "CANNOT BREAK")
})

test_that("a breaker whose single recorded subtype is complete still says CANNOT BREAK", {
  # The guard must not swallow the state for the 171 breakers whose one
  # subtype is the whole story -- suppressing everywhere to spare two
  # would trade a false negative for a useless view.
  traits <- mini_pool_ice_breaker_traits()
  traits$break_subtype_count <- 1L

  ice <- mini_pool_cardpool()[mini_pool_cardpool()$code == "ice02", ]
  brk <- mini_pool_cardpool()[mini_pool_cardpool()$code == "brk01", ]
  state <- matchup_pair_state(ice, brk, build_mini_matchup()[0, ], traits)

  expect_equal(state$kind, "cannot_break")
  expect_match(as.character(stat_strip_ui(state)), "CANNOT BREAK")
})

test_that("a release predating the count keeps the definite negative rather than losing it", {
  # The column is absent for EVERY card on such a release, so treating the
  # absence as partial evidence would disable the state for all of them to
  # spare two. The asymmetry resolves at the next implementation re-sync.
  traits <- mini_pool_ice_breaker_traits()
  expect_false("break_subtype_count" %in% names(traits))

  ice <- mini_pool_cardpool()[mini_pool_cardpool()$code == "ice02", ]
  brk <- mini_pool_cardpool()[mini_pool_cardpool()$code == "brk01", ]
  state <- matchup_pair_state(ice, brk, build_mini_matchup()[0, ], traits)

  expect_equal(state$kind, "cannot_break")
})
