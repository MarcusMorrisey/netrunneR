#' One card's matchups against everything it can meet, as a modal
#'
#' Renders compute_ice_breaker_matchups()'s output for a SINGLE card --
#' cost_to_break / credit_differential / source already computed, never
#' re-derived here. `cards` supplies display titles by code.
#'
#' NO MODE SELECTOR AND NO CARD DROPDOWNS, unlike the version of this
#' module that was a "Matchup" tab before the lane board became the whole
#' app. Both survivors of that UI ("one ice vs all breakers", "one
#' breaker vs all ice") are the same question asked from opposite sides,
#' and which side you are on is already decided by the card you opened:
#' ice compares against breakers, a breaker against ice. A radio button
#' restating the type of the card in the title bar is a control with one
#' legal value. The dropdowns went for the same reason -- this module is
#' now entered WITH a card, from the card detail modal, so a picker could
#' only re-ask a question already answered.
#'
#' The third mode, "All vs all", is gone rather than hidden. It put all
#' 21,377 subtype-compatible pairs in one reactable, roughly a third of
#' them `not_computable` dashes. Under the Standard default that would
#' now be 1,270 rows and defensible, but it answers no question a person
#' actually has: nobody compares every ice to every breaker at once, and
#' the two single-card views are what the lane board cannot already do
#' cheaply.
#'
#' MOUNTED VIA THE MODAL ONLY. Like mod_card_detail_ui(), there is no
#' top-level UI mount -- app_server() calls only the server function, and
#' the modal built below creates the output elements. Hence there is no
#' mod_matchup_explorer_ui(): a UI function nothing calls is the same
#' unreachable-export problem this module was itself an instance of.
#'
#' Uses explicit `shiny::` prefixes for the reason given on
#' mod_card_detail_ui().
#'
#' @param id Module id.
#' @param compare_code A `reactiveVal` OWNED BY THE CALLER (app_server())
#'   holding the code of the card to compare, or NULL. Instantiate this
#'   module exactly ONCE per session, for the observer-accumulation
#'   reason documented on mod_card_detail_server().
#' @param cards The active cardpool's `card` data frame.
#' @param matchup The tibble compute_ice_breaker_matchups() returns.
#' @param selected_code The same shared reactiveVal mod_card_detail_server()
#'   reads, so clicking a counterpart card in the table opens its detail.
#' @param traits The implementation release's `ice_breaker_traits`, used
#'   ONLY to tell "this breaker breaks nothing we could read" apart from
#'   "this breaker has no legal counterpart in this format". Optional:
#'   without it the empty state stays honest but less specific, rather
#'   than guessing which of the two it is.
#' @param legality The legality tables, or NULL. NULL disables the format
#'   selector entirely and leaves every pair visible, rather than
#'   silently showing an unfiltered pool as though a format had been
#'   applied -- the same contract as mod_card_browser_server().
#' @export
mod_matchup_explorer_server <- function(id, compare_code, cards, matchup,
                                        selected_code, traits = NULL,
                                        legality = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # This view renders BOTH cardpool data (card titles) and
    # implementation-derived data (cost_to_break/credit_differential), so
    # both gates are asserted here and both notices actually render in
    # the modal below.
    require_cardpool_disclaimer(CARDPOOL_DISCLAIMER_CONFIRMED)
    require_implementation_license_notice(IMPLEMENTATION_MIT_NOTICE_CONFIRMED)

    has_legality <- !is.null(legality) &&
      !is.null(legality$format_snapshot) && nrow(legality$format_snapshot) > 0

    # Built ONCE, and handed to the modal at build time rather than
    # pushed in afterwards with updateSelectInput(): see
    # mod_card_browser_ui() for what pushing choices from the server did
    # to a module living inside a modal.
    format_choices <- c("Any format" = "")
    if (has_legality && !is.null(legality$format)) {
      format_choices <- c(format_choices,
                          stats::setNames(legality$format$id, legality$format$name))
    }

    # Standard is the DEFAULT, for the reason browser_choices() gives: a
    # list that silently includes rotated and banned cards is the more
    # surprising of the two. The matchup table itself is deliberately
    # format-blind -- compute_ice_breaker_matchups() takes no format
    # argument and the lane board reads the same unfiltered table -- so
    # the filtering happens HERE, at display, exactly as
    # mod_card_browser_server() does it. Filtering at compute time would
    # bake a format into the manifest's cache identity and force a full
    # recompute on every format change.
    format_selected <- if (has_legality) "standard" else ""

    subject <- shiny::reactive({
      code <- compare_code()
      if (is.null(code)) return(NULL)
      row <- cards[cards$code == code, , drop = FALSE]
      if (nrow(row) == 0) NULL else row[1, , drop = FALSE]
    })

    # Which side of the pair the opened card is on. Anything that is not
    # ice is a breaker here because ice_breaker_pool() has already
    # narrowed `cards` to exactly those two kinds.
    subject_is_ice <- shiny::reactive({
      card <- subject()
      !is.null(card) && identical(card$type_code, "ice")
    })

    shiny::observeEvent(compare_code(), {
      card <- subject()
      shiny::req(!is.null(card))
      shiny::showModal(shiny::modalDialog(
        title = sprintf(
          "%s vs all %s", card$title,
          if (identical(card$type_code, "ice")) "breakers" else "ice"
        ),
        matchup_modal_body(session$ns, has_legality, format_choices, format_selected),
        size = "l", easyClose = TRUE,
        footer = shiny::actionButton(session$ns("close"), "Close")
      ))
    })

    shiny::observeEvent(input$close, {
      shiny::removeModal()
    })

    shiny::observeEvent(input$dismissed, {
      compare_code(NULL)
    }, ignoreInit = TRUE)

    # Codes playable in the chosen format, or NULL for "no filter".
    # Recomputed only when the format changes, not on every render.
    legal_codes <- shiny::reactive({
      fmt <- input$format %||% ""
      if (!has_legality || !nzchar(fmt)) return(NULL)
      snapshot <- active_snapshot(legality$format_snapshot, fmt)
      annotated <- annotate_format_legality(
        cards, snapshot, legality$card_pool_set, legality$restriction_card,
        legality$printing, legality$card_set
      )
      # NA-safe on both flags, and for the same reason as
      # mod_card_browser_server(): annotate_format_legality() leaves them
      # NA against a release promoted before the legality schema existed.
      banned <- !is.na(annotated$is_banned) & annotated$is_banned
      in_rotation <- !is.na(annotated$in_rotation) & annotated$in_rotation
      annotated$code[in_rotation & !banned]
    })

    # Every pair this card appears in, BEFORE any format filter. Kept
    # separate from the filtered set so the empty state below can tell
    # "no readable break clause" from "nothing legal in this format".
    all_pairs <- shiny::reactive({
      card <- subject()
      if (is.null(card)) return(matchup[0, , drop = FALSE])
      if (identical(card$type_code, "ice")) {
        matchup[matchup$ice_code == card$code, , drop = FALSE]
      } else {
        matchup[matchup$breaker_code == card$code, , drop = FALSE]
      }
    })

    display <- shiny::reactive({
      d <- all_pairs()
      codes <- legal_codes()
      if (!is.null(codes) && nrow(d) > 0) {
        # BOTH sides must be legal: a Standard breaker measured against
        # rotated ice is not a Standard matchup.
        d <- d[d$ice_code %in% codes & d$breaker_code %in% codes, , drop = FALSE]
      }
      if (nrow(d) == 0) return(d)
      d$ice_title     <- cards$title[match(d$ice_code, cards$code)]
      d$breaker_title <- cards$title[match(d$breaker_code, cards$code)]
      d[order(d$cost_to_break, na.last = TRUE), , drop = FALSE]
    })

    output$matchup_status <- shiny::renderUI({
      safe_render(function() {
        if (nrow(display()) > 0) return(NULL)
        alert_box(empty_matchup_reason(subject(), all_pairs(), traits), "info")
      })
    })

    output$matchup_table <- reactable::renderReactable({
      safe_render(function() {
        d <- display()
        if (nrow(d) == 0) return(NULL)

        # The opened card is the same on every row, so its own column
        # would be one value repeated; only the counterpart varies.
        counterpart_code  <- if (subject_is_ice()) "breaker_code" else "ice_code"
        counterpart_title <- if (subject_is_ice()) "breaker_title" else "ice_title"
        counterpart_label <- if (subject_is_ice()) "Breaker" else "Ice"

        d <- d[, c(counterpart_code, counterpart_title, "cost_to_break",
                   "credit_differential", "source"), drop = FALSE]

        # An NA renders as an em dash, not as an empty cell. reactable
        # prints NA as a zero-width space, which is indistinguishable
        # from a value we happen to have not filled in -- and
        # not_computable is a first-class state here, not a blank. The
        # wireframe prints "BREAK ---" for the same reason. Em dash as a
        # \u escape: R CMD check warns on non-ASCII bytes in R source.
        na_dash <- function(value) if (is.na(value)) "\u2014" else value

        # Column defs are name-keyed, and which column holds the
        # counterpart depends on the opened card's type, so the list is
        # assembled with setNames() rather than written as a literal with
        # both possible names present -- reactable warns about a colDef
        # naming a column that is not in the data.
        column_defs <- stats::setNames(
          list(
            reactable::colDef(show = FALSE),
            reactable::colDef(name = counterpart_label),
            reactable::colDef(name = "Cost to break", cell = na_dash),
            reactable::colDef(name = "Credit differential (rez - break; + favors runner)",
                              cell = na_dash),
            reactable::colDef(
              name = "Source",
              cell = function(value) {
                if (value == "not_computable") "not yet computable" else value
              }
            )
          ),
          c(counterpart_code, counterpart_title, "cost_to_break",
            "credit_differential", "source")
        )

        # THE WHOLE ROW IS THE CLICK TARGET, via reactable's own onClick
        # rather than an onclick= attribute on a tag returned from a cell
        # renderer. reactable draws cells through React, which ignores a
        # lowercase `onclick` attribute, so the span this used to return
        # arrived in the browser stripped of its handler and the table
        # was silently unclickable. Nothing caught that: the module was
        # mounted nowhere, and a server-side test can set the input
        # directly without ever going through the DOM. The other views
        # (mod_card_browser, mod_lane_board) use click_sets_input() to
        # this day because they render plain tags through renderUI(),
        # where the attribute survives.
        #
        # A row rather than the name alone, which the earlier version
        # needed because a row was a PAIR and clicking it was ambiguous
        # about which card to open. Only the counterpart varies now, so
        # the ambiguity is gone with it.
        reactable::reactable(
          d, columns = column_defs, defaultSorted = "cost_to_break",
          onClick = htmlwidgets::JS(sprintf(
            "function(rowInfo) { Shiny.setInputValue('%s', rowInfo.row['%s'], {priority: 'event'}); }",
            session$ns("row_card_clicked"), counterpart_code
          )),
          rowStyle = list(cursor = "pointer")
        )
      })
    })

    # Opening the counterpart's detail REPLACES this modal rather than
    # stacking a second one on top of it: Bootstrap handles nested modals
    # poorly, and the detail modal's own dismissal handler is bound to
    # the same '#shiny-modal' element this one uses. compare_code is
    # cleared first so that returning to this card later still fires the
    # observer above, which only reacts to a change.
    shiny::observeEvent(input$row_card_clicked, {
      compare_code(NULL)
      shiny::removeModal()
      selected_code(input$row_card_clicked)
    })

    # Bound at session start, but their elements exist only while the
    # modal is open. See mod_card_detail_server(): Shiny suspends an
    # output whose element it cannot see and does not reliably resume it,
    # which presents as a table stuck recalculating with the R process
    # idle at ~0% CPU.
    shiny::outputOptions(output, "matchup_status", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "matchup_table", suspendWhenHidden = FALSE)
  })
}

#' Everything inside the matchup modal, as one tagList
#'
#' Factored out of the showModal() call ABOVE ALL so the pre-ship gate in
#' test-preship-gates.R can still reach it. That gate asserts that a view
#' claiming a licence notice actually reproduces one, and it exists
#' because this very module once named the MIT licence without
#' reproducing it. When the module was a tab the gate could squish
#' mod_matchup_explorer_ui(); now that the view is built inside a modal
#' by the server, a function returning the same tags is what keeps the
#' assertion possible without a browser.
#'
#' Takes `ns` rather than a session so it can be called with a bare
#' shiny::NS(), with no session to mock.
#'
#' @param ns A namespace function, from `shiny::NS()` or `session$ns`.
#' @param has_legality Logical. FALSE omits the format selector entirely
#'   rather than offering one with only "Any format" in it.
#' @param format_choices,format_selected The named character vector of
#'   format ids and the id to select, as built by the server.
#' @return A `shiny::tagList()`.
#' @keywords internal
matchup_modal_body <- function(ns, has_legality, format_choices, format_selected) {
  shiny::tagList(
    # Identifies WHICH modal this is, so the dismissal handler below only
    # fires for its own -- that handler is delegated on `document`, so
    # without this it also fires for the card detail modal. See the
    # fuller account in mod_card_detail_server().
    shiny::div(class = "dc-matchup-modal", style = "display: none;"),
    if (has_legality) {
      shiny::selectInput(ns("format"), "Format",
                         choices = format_choices, selected = format_selected)
    },
    shiny::uiOutput(ns("matchup_status")),
    reactable::reactableOutput(ns("matchup_table")),
    # Both notices render because both kinds of data are on screen: card
    # titles from cardpool, cost_to_break/credit_differential from the
    # implementation lineage.
    implementation_mit_notice_ui(),
    cardpool_disclaimer_ui(),
    # Cleared on EVERY dismissal path, for the reason spelled out at
    # length in mod_card_detail_server() -- without this, reopening the
    # same card straight after an Escape/backdrop dismissal no-ops, since
    # the observer only fires on a *change*.
    shiny::tags$script(shiny::HTML(sprintf(
      "$(document).off('hidden.bs.modal.dcMatchup').on('hidden.bs.modal.dcMatchup', '#shiny-modal', function() {
        if (!$(this).find('.dc-matchup-modal').length) { return; }
        Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
      });",
      ns("dismissed")
    )))
  )
}

#' Say WHY there is nothing to show, not just that there is nothing
#'
#' Three different situations reach an empty table and they are not the
#' same answer. A breaker whose break clause the implementation parser
#' could not read has no computable matchup against anything, which is a
#' fact about our data rather than about the card; a card whose pairs the
#' format filter removed has matchups, just not in this format; and
#' anything else is left unexplained rather than guessed at.
#'
#' `traits` is optional, so the unreadable-break-clause case degrades to
#' the generic message instead of being asserted without evidence. The
#' generic message deliberately does not say "this card breaks nothing":
#' 36 of the 173 icebreakers in the pool have an unparsed `break_subtype`
#' and breaker_matches_ice() treats that as FALSE, so a bare "no
#' matchups" would report our parser's gap as a property of the card.
#'
#' @param card One row of the cardpool `card` frame, or NULL.
#' @param pairs That card's matchup rows BEFORE format filtering.
#' @param traits The `ice_breaker_traits` table, or NULL.
#' @return A character scalar.
#' @keywords internal
empty_matchup_reason <- function(card, pairs, traits = NULL) {
  if (is.null(card)) return("No card selected.")

  if (nrow(pairs) > 0) {
    return("No matchups in this format. Try 'Any format'.")
  }

  if (!is.null(traits) && !identical(card$type_code, "ice")) {
    row <- traits[traits$code == card$code, , drop = FALSE]
    if (nrow(row) == 1 && is.na(row$break_subtype[[1]])) {
      return(paste(
        "This card's break clause could not be read from the implementation",
        "source, so no matchup can be computed for it. That is a gap in our",
        "data, not a claim that it breaks nothing."
      ))
    }
  }

  "No matchups for this card."
}
