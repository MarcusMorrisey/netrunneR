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
#' ice x breaker cross join. `mod_matchup_explorer` answers the
#' complementary question -- everything ONE card can meet, sortable by
#' cost -- and is reached from the card detail modal rather than as a
#' peer view; this view is what the wireframe specifies as the landing
#' screen.
#'
#' This board reads the matchup table UNFILTERED by format, unlike the
#' matchup modal and the card browser, which both default to Standard. A
#' lane can therefore pair cards that no format allows together. That is
#' a real gap rather than a decision -- see inst/shiny-app/CLAUDE.md.
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
#' NOTHING IS PAINTED OVER THE CARD. An earlier version drew a title
#' band and a cost pip on top of each image, carried over from the
#' wireframe -- where the tile was an art crop with no card text on it.
#' Now that whole cards are shown, every one of those values is already
#' printed on the card itself, in the publisher's own layout, and the
#' overlay only covered the art it duplicated. The title survives as the
#' element's `title` and the image's `alt`, which is where a name belongs
#' for a clickable image anyway.
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
#' The wireframe's top strip advertises sibling apps. "Meta Maps" now
#' exists and is real navigation; "Meta Stats" still does not, and stays
#' an inert label rather than a link that would go nowhere.
#'
#' SETS A NON-NAMESPACED INPUT. Every other click in this file is
#' namespaced to a module, because it belongs to that module. This one
#' does not: the strip switches which VIEW the app shows, which is
#' app_server()'s business and not the lane board's. The strip is drawn
#' here only because the wireframe puts it above the board.
#'
#' @param active Character. Key of the current view.
#' @keywords internal
suite_nav_ui <- function(active = "iceBreaker") {
  built <- c("iceBreaker", "metaMaps")
  item <- function(key, label) {
    is_active <- identical(key, active)
    is_built <- key %in% built
    shiny::tags$span(
      class = paste(
        "nr-suite-item",
        if (is_active) "nr-suite-item-active"
        else if (is_built) "nr-suite-item-link"
        else "nr-suite-item-inert"
      ),
      title = if (is_active || is_built) NULL else "Not built yet",
      onclick = if (!is_active && is_built) {
        sprintf("Shiny.setInputValue('nav_view', '%s', {priority: 'event'})", key)
      },
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
#' @param traits The implementation release's `ice_breaker_traits`, used
#'   ONLY to tell a pair the subtype filter removed ON PURPOSE from one it
#'   removed because the breaker could not be read. Optional: without it
#'   both collapse to "not computable", which is what this module said
#'   before it could tell them apart -- weaker, but not wrong.
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
                                  traits = NULL,
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

    # Pairs the operator has asserted are breakable despite the subtypes
    # saying otherwise, as "<ice_code>|<breaker_code>" keys -- the same
    # composite key remove_breaker already uses, because the same breaker
    # can sit in several lanes and an assertion about one lane is not an
    # assertion about the others.
    #
    # SESSION-ONLY, deliberately. An override here means "in the game I am
    # looking at, something has changed what this ice is" -- a
    # Deep Data Mining giving it a subtype, an AI breaker effect, a
    # one-off. That is a statement about a board state, not about the
    # cards, so it must not outlive the session or leak into
    # matchup_overrides.csv, which is for corrections that are true of the
    # cards themselves.
    overridden <- shiny::reactiveVal(character(0))

    output$lanes <- shiny::renderUI({
      safe_render(function() {
        codes <- ice_codes()
        if (length(codes) == 0) return(empty_board_ui(session))
        by_ice <- breakers()
        shiny::div(
          class = "nr-lanes",
          lapply(codes, function(ice_code) {
            lane_ui(session, ice_code, by_ice[[ice_code]] %||% character(0),
                    cards, matchup, traits, overridden())
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

    # Removing an ice takes its lane, and its lane's breakers with it --
    # those breakers were chosen against THAT ice, so keeping them
    # orphaned would leave a stack of cards with nothing to compare to.
    shiny::observeEvent(input$remove_ice, {
      code <- input$remove_ice
      ice_codes(setdiff(ice_codes(), code))
      by_ice <- breakers()
      by_ice[[code]] <- NULL
      breakers(by_ice)
    })

    # Toggles rather than sets, so the same control turns the assumption
    # back off -- an override you cannot withdraw is a trap.
    shiny::observeEvent(input$override_toggled, {
      key <- input$override_toggled
      current <- overridden()
      overridden(if (key %in% current) setdiff(current, key) else c(current, key))
    })

    shiny::observeEvent(input$remove_breaker, {
      parts <- strsplit(input$remove_breaker, "|", fixed = TRUE)[[1]]
      if (length(parts) != 2L) return(invisible(NULL))
      by_ice <- breakers()
      by_ice[[parts[[1]]]] <- setdiff(by_ice[[parts[[1]]]] %||% character(0), parts[[2]])
      breakers(by_ice)
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
lane_ui <- function(session, ice_code, breaker_codes, cards, matchup,
                    traits = NULL, overridden = character(0)) {
  ice <- cards[cards$code == ice_code, ]
  if (nrow(ice) == 0) return(NULL)

  shiny::div(
    class = "nr-lane",
    ice_card_ui(session, ice),
    lapply(breaker_codes, function(bc) {
      breaker <- cards[cards$code == bc, ]
      if (nrow(breaker) == 0) return(NULL)
      pair <- matchup[matchup$ice_code == ice_code & matchup$breaker_code == bc, ]
      state <- matchup_pair_state(ice, breaker, pair, traits, overridden)
      shiny::tagList(
        breaker_card_ui(session, breaker, ice_code),
        stat_strip_ui(state, session)
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
  shiny::div(
    class = "nr-lane-card nr-lane-card-ice",
    # The title is the accessible name for a clickable image with no text
    # of its own; it is also what a hover reveals, which is why the card
    # needs no caption painted over it.
    title = ice$title,
    onclick = click_sets_input(session, "card_clicked", ice$code),
    shiny::tags$img(class = "nr-lane-art", src = card_image_url(ice$code),
                    loading = "lazy", alt = ice$title),
    shiny::div(class = "nr-lane-hatch"),
    remove_button(session, "remove_ice", ice$code,
                  sprintf("Remove %s", ice$title))
  )
}

#' A portrait breaker card
#' @keywords internal
breaker_card_ui <- function(session, breaker, ice_code) {
  shiny::div(
    class = "nr-lane-card nr-lane-card-breaker",
    title = breaker$title,
    onclick = click_sets_input(session, "card_clicked", breaker$code),
    shiny::tags$img(class = "nr-lane-art", src = card_image_url(breaker$code),
                    loading = "lazy", alt = breaker$title),
    # Carries BOTH codes: the same breaker can sit in several lanes, and
    # removing it from one must not disturb the others.
    remove_button(session, "remove_breaker",
                  paste(ice_code, breaker$code, sep = "|"),
                  sprintf("Remove %s from this lane", breaker$title))
  )
}

#' A remove control for one card
#'
#' Revealed on hover and on keyboard focus, never painted over the card at
#' rest. The cards carry the publisher's own artwork and text, and a
#' control sitting permanently on top of that is exactly what was taken
#' off them; appearing only when the pointer is on the card keeps the
#' affordance without the clutter.
#'
#' `event.stopPropagation()` matters: the card underneath opens the detail
#' modal on click, so without it removing a card would also open the card
#' being removed.
#'
#' @param session The module session.
#' @param input_id Character. Unnamespaced input to set.
#' @param value Character. Value identifying what to remove.
#' @param label Character. Accessible name and tooltip.
#' @keywords internal
remove_button <- function(session, input_id, value, label) {
  shiny::tags$button(
    class = "nr-remove",
    type = "button",
    title = label,
    `aria-label` = label,
    onclick = paste0("event.stopPropagation(); ",
                     click_sets_input(session, input_id, value)),
    # Multiplication sign, not the letter x: R CMD check warns on
    # non-ASCII bytes in source outside comments.
    "\u00d7"
  )
}

#' What the strip under one breaker is actually saying
#'
#' compute_ice_breaker_matchups() emits a row only for SUBTYPE-COMPATIBLE
#' pairs, so on this board -- where the operator stacks whatever cards
#' they like -- an absent row is the common case and it has two entirely
#' different meanings:
#'
#' * the breaker declares what it breaks and this ice is not that, so the
#'   filter dropped the pair ON PURPOSE. That is knowledge, and the
#'   strongest kind: a Fracter cannot break a Code Gate, ever. Saying
#'   "not computable" here understates what we know to nothing.
#' * the breaker's break clause could not be read at all, so we do not
#'   know what it breaks. That really is "we cannot tell you".
#'
#' This module used to render both as `not_computable`, on the reasoning
#' that an absent row and an uncomputable one are equally "we cannot tell
#' you". That holds only while nothing can distinguish them; `traits` can.
#'
#' The first case is OVERRIDABLE, because subtype is not immutable during
#' a game -- effects add subtypes to ice, and an AI breaker or a one-off
#' can break something its printed clause does not name. The override is
#' an assertion by the operator about the board in front of them, so the
#' arithmetic it unlocks is badged `assumed` rather than `formula`: the
#' numbers are real, the premise is theirs.
#'
#' @param ice,breaker One-row card frames.
#' @param pair That pair's matchup row, or a zero-row frame.
#' @param traits `ice_breaker_traits`, or NULL. NULL collapses the two
#'   absent-row cases back into one, rather than guessing which applies.
#' @param overridden Character vector of "<ice>|<breaker>" keys.
#' @return A list with `kind` ("known", "cannot_break", "assumed",
#'   "unknown"), `pair` (a matchup-shaped row, possibly zero-row), `key`
#'   and `overridable`.
#' @keywords internal
matchup_pair_state <- function(ice, breaker, pair, traits = NULL,
                               overridden = character(0)) {
  key <- paste(ice$code, breaker$code, sep = "|")
  out <- function(kind, pair, overridable = FALSE) {
    list(kind = kind, pair = pair, key = key, overridable = overridable,
         overridden = key %in% overridden)
  }

  if (nrow(pair) == 1) return(out("known", pair))
  if (is.null(traits)) return(out("unknown", pair))

  bt <- traits[traits$code == breaker$code, , drop = FALSE]
  subtype <- if (nrow(bt) == 1) bt$break_subtype[[1]] else NA_character_
  # No readable break clause: we do not know what it breaks, so we cannot
  # say it cannot break this.
  if (is.na(subtype)) return(out("unknown", pair))

  # Asked rather than inferred from the row's absence. The two agree, but
  # a reader should not have to reconstruct the filter's reasoning from a
  # missing row to see why this says what it says.
  if (breaker_matches_ice(ice$keywords, subtype)) return(out("unknown", pair))

  # A DEFINITE NEGATIVE NEEDS COMPLETE EVIDENCE, and for two cards it is
  # not complete. A defcard may carry several break clauses naming
  # different subtypes, and ice_breaker_traits records only one: Penrose
  # breaks Code Gates and also Barriers the turn it is installed,
  # Lobisomem breaks Code Gates and X Barrier subroutines. The dropped
  # subtype's pairings are absent from the matchup table, and this
  # function would read that absence as "cannot break" -- stating
  # confidently that Penrose cannot break a Code Gate, which is false for
  # 133 pairings, and the same for 101 of Lobisomem's.
  #
  # So where the card is KNOWN to declare more subtypes than we stored,
  # the answer falls back to "we do not know". That is the honest reading
  # of partial evidence, and it is what this rendered before the
  # cannot_break state existed.
  #
  # This is a guard, not the fix. Recording every clause with its own
  # subtype and cost is the fix; until then this stops us asserting
  # something false. See break_subtype_count() in R/build-implementation.R.
  #
  # A release predating the column leaves the count NA, and that case
  # KEEPS the definite negative rather than suppressing it everywhere:
  # the column is absent for every card on such a release, so treating NA
  # as incomplete would disable the state for all 171 breakers whose
  # single recorded subtype is complete, in order to spare 2. The
  # asymmetry resolves itself the moment the implementation lineage is
  # re-synced, after which the count is always present.
  count <- if ("break_subtype_count" %in% names(bt)) bt$break_subtype_count[[1]] else NA_integer_
  if (!is.na(count) && count > 1L) return(out("unknown", pair))

  if (!(key %in% overridden)) return(out("cannot_break", pair, overridable = TRUE))
  out("assumed", assumed_pair_cost(ice, breaker, traits), overridable = TRUE)
}

#' Price a pair the subtype filter excluded, on the operator's say-so
#'
#' Runs exactly the same arithmetic compute_ice_breaker_matchups() would
#' have, for the one pair it declined to emit. Nothing here reaches into
#' the matchup table or writes to it: the assumption belongs to one
#' session and one lane, and baking it into the shared table would make
#' one operator's board state everybody's data.
#'
#' `source` comes back "assumed", never "formula". The subtraction is the
#' same either way; what differs is that a person supplied the premise,
#' and the badge has to keep saying so.
#'
#' @param ice,breaker One-row card frames.
#' @param traits `ice_breaker_traits`.
#' @return A one-row matchup-shaped tibble.
#' @keywords internal
assumed_pair_cost <- function(ice, breaker, traits) {
  it <- traits[traits$code == ice$code, , drop = FALSE]
  bt <- traits[traits$code == breaker$code, , drop = FALSE]

  empty <- tibble::tibble(
    ice_code = ice$code, breaker_code = breaker$code,
    cost_to_break = NA_integer_, stealth_credits = NA_integer_,
    credit_differential = NA_integer_, source = "not_computable"
  )
  if (nrow(it) != 1 || nrow(bt) != 1) return(empty)

  # Same tolerance as compute_ice_breaker_matchups(): a release built
  # before these columns existed must degrade to NA, not abort a render.
  bt <- fill_missing_columns(bt, list(
    pump_stealth = NA_integer_,
    pump_resource_type = NA_character_,
    pump_resource_qty = NA_integer_
  ))

  cost <- compute_cost_to_break_formula(
    ice$strength, it$subroutine_count, breaker$strength,
    bt$break_cost, bt$break_qty, bt$pump_cost, bt$pump_amount,
    bt$pump_resource_type
  )
  if (is.na(cost)) return(empty)

  pumps <- strength_pump_applications(ice$strength, breaker$strength, bt$pump_amount)
  stealth <- if (is.na(bt$pump_stealth) || is.na(pumps) || pumps == 0L) {
    NA_integer_
  } else {
    as.integer(pumps * bt$pump_stealth)
  }

  tibble::tibble(
    ice_code = ice$code, breaker_code = breaker$code,
    cost_to_break = as.integer(cost),
    stealth_credits = stealth,
    credit_differential = as.integer(ice$cost - cost),
    source = "assumed"
  )
}

#' The stat strip under a breaker: break cost, differential, provenance
#'
#' `source` is rendered as a badge rather than a bare word because the
#' states mean genuinely different things to a reader: "formula" is
#' derived, "override" is hand-curated, "assumed" is derived from a
#' premise the operator supplied, "cannot break" is a definite negative,
#' and "not computable" is the honest admission that we do not know.
#'
#' Nothing here collapses to a blank: a blank strip would read as "zero
#' cost to break", which is the opposite of every one of those.
#'
#' CANNOT BREAK IS NOT NOT-COMPUTABLE. This used to render an absent
#' matchup row as `not_computable`, reasoning that a missing pair and an
#' uncomputable one are equally "we cannot tell you". They are not: the
#' subtype filter drops a Fracter/Code Gate pair on purpose, and that is
#' the most definite thing this app can say about a pairing. See
#' matchup_pair_state(), which is where the two are told apart.
#'
#' @param state The list matchup_pair_state() returns.
#' @param session The module session, for the override control's
#'   click_sets_input(). NULL renders the strip without that control,
#'   which is what a test asserting only the text wants.
#' @keywords internal
stat_strip_ui <- function(state, session = NULL) {
  dash <- "\u2014"
  minus <- "\u2212"

  pair <- state$pair
  src <- if (identical(state$kind, "cannot_break")) {
    "cannot_break"
  } else if (nrow(pair) == 1) {
    pair$source
  } else {
    "not_computable"
  }
  known <- src %in% c("formula", "override", "assumed")

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
    assumed  = shiny::tags$span(class = "nr-badge nr-badge-assumed", "ASSUMED"),
    cannot_break = shiny::tags$span(class = "nr-badge nr-badge-cannot", "CANNOT BREAK"),
    shiny::tags$span(class = "nr-badge nr-badge-unknown", "NOT COMPUTABLE")
  )

  shiny::tagList(
    shiny::div(
      class = "nr-strip",
      shiny::tags$span(class = "nr-strip-break", "BREAK ", shiny::tags$b(break_cost)),
      diff_ui,
      badge
    ),
    # Only where the subtypes are what stands in the way. There is nothing
    # for an operator to assert about a breaker whose break clause we
    # could not read in the first place -- overriding that would just
    # produce NA under a badge claiming a premise.
    if (isTRUE(state$overridable) && !is.null(session)) {
      override_control_ui(session, state)
    }
  )
}

#' The "assume it breaks this" control under an incompatible pair
#'
#' A real checkbox, not a link, because it is a persistent two-state
#' assertion about this lane rather than an action -- and because its
#' state has to survive the board re-rendering, which it does by being
#' redrawn from `overridden` each time rather than by the browser
#' remembering it.
#'
#' `event.stopPropagation()` for the same reason remove_button() needs it:
#' the strip sits under a card whose own onclick opens the card detail
#' modal, and a click that both ticks the box and opens a modal is a
#' click that did something the operator did not ask for.
#' @keywords internal
override_control_ui <- function(session, state) {
  shiny::tags$label(
    class = "nr-override",
    shiny::tags$input(
      type = "checkbox",
      # NULL omits the attribute entirely; "checked" is not a value that
      # can be set to false.
      checked = if (isTRUE(state$overridden)) "checked",
      onclick = paste0("event.stopPropagation(); ",
                       click_sets_input(session, "override_toggled", state$key))
    ),
    shiny::tags$span(
      if (isTRUE(state$overridden)) "assuming it breaks this" else "compute anyway"
    )
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
