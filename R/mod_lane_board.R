#' The lane board: the app's main view
#'
#' Implements `Main.dc.html` from
#' homelab/docs/netrunneR/design-references/wireframes/. Each selected ice
#' anchors a lane as a landscape card; the shared breaker set stacks below
#' it as portrait cards, each followed by a stat strip carrying that
#' (ice, breaker) pair's `cost_to_break`, `credit_differential` and
#' `source` badge.
#'
#' WHY LANES RATHER THAN A TABLE: the question this app exists to answer
#' is "which of my breakers handles this ice, and at what cost" -- that is
#' a comparison across a handful of chosen cards, not a scan of the full
#' ice x breaker cross join. `mod_matchup_explorer` renders the whole
#' matchup tibble as one sortable table, which answers a different
#' question and is kept for that purpose; this view is what the wireframe
#' specifies as the landing screen.
#'
#' BREAKERS ARE PER LANE. Each ice gets its own stack, and each lane's
#' "+" adds to that lane only. An earlier draft shared one breaker set
#' across every lane, on the theory that stat strips are only comparable
#' when the breaker rows line up -- that was wrong twice over. Cards and
#' strips are fixed-size, so the Nth row of every lane sits at the same
#' height either way; and reading ACROSS rows is not the question this
#' board answers. It answers "for this ice, which of my breakers handles
#' it, and at what cost", which is read down a single lane.
#'
#' This view renders BOTH cardpool data (titles, images, strength,
#' keywords) and implementation-derived data (cost_to_break /
#' credit_differential), so it renders -- and its server guards -- both
#' the cardpool disclaimer and the mtgred/netrunner MIT notice. Explicit
#' `shiny::` prefixes throughout, for the reason given on
#' `mod_card_detail_ui()`.
#'
#' @param id Module id.
#' @export
mod_lane_board_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    suite_nav_ui("iceBreaker"),
    shiny::div(
      class = "nr-appbar",
      shiny::div(
        class = "nr-appbar-title",
        shiny::tags$span(class = "nr-appbar-mark", "\u25a3"),
        "Ice ", shiny::tags$span(class = "nr-swap", "\u21c4"), " Breaker"
      ),
      shiny::div(
        class = "nr-appbar-actions",
        # LOAD DECK is deliberately disabled rather than absent: the
        # wireframe specifies it, but the decklist tables it needs were
        # intentionally dropped from the nrdb mirror (see
        # docs/netrunneR/offline-mirror-plan.md), so there is no local
        # data behind it. Rendering it dead states the gap; removing it
        # would quietly lose the design intent.
        shiny::tags$button(
          class = "nr-btn", disabled = NA,
          title = "Needs decklist data, which this mirror does not carry",
          "LOAD DECK"
        ),
        shiny::actionButton(ns("add_ice"), "ADD ICE", class = "nr-btn nr-btn-accent")
      )
    ),
    shiny::div(class = "nr-board", shiny::uiOutput(ns("lanes"))),
    shiny::div(
      class = "nr-footer",
      cardpool_disclaimer_ui(),
      implementation_mit_notice_ui()
    )
  )
}

#' The suite nav strip
#'
#' The wireframe's top strip advertises sibling apps ("Meta Maps", "Meta
#' Stats") that do not exist. They are rendered as plainly inert labels
#' rather than links, so the strip reads as the wireframe drew it without
#' implying navigation that would 404.
#'
#' @param active Character. Key of the current app.
#' @keywords internal
suite_nav_ui <- function(active = "iceBreaker") {
  item <- function(key, label) {
    shiny::tags$span(
      class = if (identical(key, active)) {
        "nr-suite-item nr-suite-item-active"
      } else {
        "nr-suite-item nr-suite-item-inert"
      },
      title = if (identical(key, active)) NULL else "Not built yet",
      label
    )
  }
  shiny::div(
    class = "nr-suite",
    shiny::tags$span(class = "nr-suite-brand", "NETRUNNER TOOLS"),
    shiny::div(class = "nr-suite-rule"),
    shiny::div(
      class = "nr-suite-items",
      item("iceBreaker", "Ice::Breaker"),
      item("metaMaps", "Meta Maps"),
      item("metaStats", "Meta Stats")
    )
  )
}

#' Lane board module server
#'
#' @param id Module id.
#' @param cards The active cardpool's `card` data frame.
#' @param matchup The tibble `compute_ice_breaker_matchups()` returns.
#' @param selected_code The shared card-detail `reactiveVal` owned by
#'   `app_server()`; this module only ever sets it.
#' @param on_add_ice Zero-argument function invoked when the operator asks
#'   to add an ice. `app_server()` supplies the one that opens the search
#'   modal, so this module does not need to know the modal exists.
#' @param on_add_breaker Zero-argument function, the breaker counterpart.
#' @return A list of two functions, `add_ice(code)` and
#'   `add_breaker(code)`, so the caller can push a card chosen elsewhere
#'   into the board. The board owns its own contents; the search modal
#'   only names a card.
#' @export
mod_lane_board_server <- function(id, cards, matchup, selected_code,
                                  on_add_ice = function() NULL,
                                  on_add_breaker = function() NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    require_cardpool_disclaimer(CARDPOOL_DISCLAIMER_CONFIRMED)
    require_implementation_license_notice(IMPLEMENTATION_MIT_NOTICE_CONFIRMED)

    ice_codes <- shiny::reactiveVal(character(0))
    # Breakers are held PER LANE: a named list, ice code -> the breakers
    # stacked under that ice. Each lane's "+" adds to that lane and no
    # other, which is what the wireframe draws and the only reading in
    # which the button means what it looks like it means.
    breakers <- shiny::reactiveVal(list())
    # Which lane's "+" was last pressed, so the code coming back from the
    # search modal knows where to land. Set on the click, read on the
    # pick; the modal itself stays ignorant of lanes.
    pending_lane <- shiny::reactiveVal(NULL)

    output$lanes <- shiny::renderUI({
      safe_render(function() {
        codes <- ice_codes()
        if (length(codes) == 0) return(empty_board_ui(session))
        by_ice <- breakers()
        shiny::div(
          class = "nr-lanes",
          lapply(codes, function(ice_code) {
            lane_ui(session, ice_code, by_ice[[ice_code]] %||% character(0),
                    cards, matchup)
          }),
          add_lane_ui(session)
        )
      })
    })

    shiny::observeEvent(input$add_ice, on_add_ice())

    shiny::observeEvent(input$add_breaker, {
      # The slot carries its own lane's ice code as the click value.
      pending_lane(input$add_breaker)
      on_add_breaker()
    })

    shiny::observeEvent(input$card_clicked, {
      selected_code(input$card_clicked)
    })

    list(
      add_ice = function(code) {
        if (!code %in% ice_codes()) ice_codes(c(ice_codes(), code))
      },
      add_breaker = function(code) {
        lane <- pending_lane()
        # No pending lane means nothing asked for a breaker -- dropping
        # the pick is better than guessing a lane to put it in.
        if (is.null(lane)) return(invisible(NULL))
        by_ice <- breakers()
        current <- by_ice[[lane]] %||% character(0)
        if (!code %in% current) {
          by_ice[[lane]] <- c(current, code)
          breakers(by_ice)
        }
      }
    )
  })
}

#' One lane: an ice card, then every breaker with its stat strip
#' @keywords internal
lane_ui <- function(session, ice_code, breaker_codes, cards, matchup) {
  ice <- cards[cards$code == ice_code, ]
  if (nrow(ice) == 0) return(NULL)

  shiny::div(
    class = "nr-lane",
    ice_card_ui(session, ice),
    lapply(breaker_codes, function(bc) {
      breaker <- cards[cards$code == bc, ]
      if (nrow(breaker) == 0) return(NULL)
      pair <- matchup[matchup$ice_code == ice_code & matchup$breaker_code == bc, ]
      shiny::tagList(
        breaker_card_ui(session, breaker),
        stat_strip_ui(pair)
      )
    }),
    shiny::div(
      class = "nr-slot nr-slot-portrait",
      onclick = click_sets_input(session, "add_breaker", ice_code),
      shiny::tags$span(class = "nr-slot-plus", "+")
    )
  )
}

#' The landscape ice card that anchors a lane
#' @keywords internal
ice_card_ui <- function(session, ice) {
  # Middle dot as a \u escape, not the literal character: R CMD check
  # warns on any non-ASCII byte in R source outside comments. Same for
  # the em dash standing in for an absent value.
  separator <- "\u00b7"
  dash <- "\u2014"
  subtitle <- paste(
    toupper(if (is.na(ice$keywords)) ice$type_code else ice$keywords),
    separator,
    paste0("STR ", if (is.na(ice$strength)) dash else ice$strength)
  )
  shiny::div(
    class = "nr-lane-card nr-lane-card-ice",
    onclick = click_sets_input(session, "card_clicked", ice$code),
    shiny::tags$img(class = "nr-lane-art", src = card_image_url(ice$code),
                    loading = "lazy", alt = ""),
    shiny::div(class = "nr-lane-hatch"),
    cost_pip_ui(ice$cost),
    shiny::div(
      class = "nr-lane-label",
      shiny::div(class = "nr-lane-title", ice$title),
      shiny::div(class = "nr-lane-sub", subtitle)
    )
  )
}

#' A portrait breaker card
#' @keywords internal
breaker_card_ui <- function(session, breaker) {
  shiny::div(
    class = "nr-lane-card nr-lane-card-breaker",
    onclick = click_sets_input(session, "card_clicked", breaker$code),
    shiny::tags$img(class = "nr-lane-art", src = card_image_url(breaker$code),
                    loading = "lazy", alt = ""),
    cost_pip_ui(breaker$cost),
    shiny::div(
      class = "nr-lane-label",
      shiny::div(class = "nr-lane-title", breaker$title),
      shiny::div(class = "nr-lane-sub", "ICEBREAKER")
    )
  )
}

#' The cost pip in a card's top-left corner
#' @keywords internal
cost_pip_ui <- function(cost) {
  shiny::div(
    class = "nr-pip",
    if (length(cost) == 0 || is.na(cost)) "\u2014" else as.character(cost)
  )
}

#' The stat strip under a breaker: break cost, differential, provenance
#'
#' `source` is rendered as a badge rather than a bare word because the
#' three states mean genuinely different things to a reader: "formula" is
#' derived, "override" is hand-curated, and "not_computable" is the honest
#' admission that `compute_cost_to_break_formula()` is still a stub for
#' this pair (see R/views-matchups.R). An absent row is the same statement
#' as `not_computable`, so it renders identically rather than collapsing
#' to a blank -- a missing pair and an uncomputable one are equally "we
#' cannot tell you", and showing nothing would read as "zero".
#' @keywords internal
stat_strip_ui <- function(pair) {
  dash <- "\u2014"
  minus <- "\u2212"

  src <- if (nrow(pair) == 1) pair$source else "not_computable"
  known <- !identical(src, "not_computable")

  break_cost <- if (known && !is.na(pair$cost_to_break)) {
    as.character(pair$cost_to_break)
  } else {
    dash
  }
  diff_value <- if (known && !is.na(pair$credit_differential)) {
    pair$credit_differential
  } else {
    NA_integer_
  }

  diff_ui <- if (is.na(diff_value)) {
    shiny::tags$span(class = "nr-diff nr-diff-none", dash)
  } else {
    shiny::tags$span(
      class = paste("nr-diff", if (diff_value >= 0) "nr-diff-good" else "nr-diff-bad"),
      if (diff_value >= 0) paste0("+", diff_value) else paste0(minus, abs(diff_value))
    )
  }

  badge <- switch(src,
    formula  = shiny::tags$span(class = "nr-badge nr-badge-formula", "FORMULA"),
    override = shiny::tags$span(class = "nr-badge nr-badge-override", "OVERRIDE"),
    shiny::tags$span(class = "nr-badge nr-badge-unknown", "NOT COMPUTABLE")
  )

  shiny::div(
    class = "nr-strip",
    shiny::tags$span(class = "nr-strip-break", "BREAK ", shiny::tags$b(break_cost)),
    diff_ui,
    badge
  )
}

#' The dashed "add another ice" lane
#' @keywords internal
add_lane_ui <- function(session) {
  shiny::div(
    class = "nr-lane",
    shiny::div(
      class = "nr-slot nr-slot-landscape",
      onclick = click_sets_input(session, "add_ice", "lane"),
      shiny::tags$span(class = "nr-slot-plus", "+"),
      shiny::tags$span(class = "nr-slot-text", "ADD ICE TO COMPARE")
    )
  )
}

#' The board before any ice has been chosen
#' @keywords internal
empty_board_ui <- function(session) {
  shiny::div(
    class = "nr-lanes",
    shiny::div(
      class = "nr-lane",
      shiny::div(
        class = "nr-slot nr-slot-landscape",
        onclick = click_sets_input(session, "add_ice", "first"),
        shiny::tags$span(class = "nr-slot-plus", "+"),
        shiny::tags$span(class = "nr-slot-text", "ADD ICE TO COMPARE")
      )
    )
  )
}
