#' Shared component: image + stats + text for one card, invoked as a modal.
#'
#' Exported because `inst/shiny-app/app.R` calls this from outside the
#' package namespace -- `inst/` files are sourced by
#' `shiny::shinyAppDir()` separately from the package's own namespace, so
#' anything they call must be reachable via `netrunneR::`, not a bare
#' name.
#'
#' shiny is referenced with explicit `shiny::` prefixes throughout this
#' file (rather than an `@import shiny`), matching how every other
#' package in this codebase's Imports is used -- only rlang's `abort` and
#' `.data` are imported by name, in R/netrunneR-package.R.
#'
#' @param id Module id.
#' @export
mod_card_detail_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("detail"))
}

#' Card detail modal module server
#'
#' @param id Module id.
#' @param selected_code A `reactiveVal` OWNED BY THE CALLER (app_server(),
#'   not this module) holding the currently selected card code, or NULL.
#'   This module must be instantiated exactly ONCE per session and only
#'   reacts to `selected_code()` changing -- calling `moduleServer()`
#'   fresh per card click would accumulate a growing set of observers per
#'   session, never releasing the previous set.
#' @param cards The active cardpool's `card` data frame.
#' @param rulings The active nrdb release's `ruling` table, or NULL when
#'   no nrdb release is active. Optional: the panel is omitted rather
#'   than the modal failing, since this app is about ice/breaker
#'   economics and rulings are an augmentation of it.
#' @param legality The legality tables, used only to trace this card to
#'   the publisher that released it, so the modal shows that card's own
#'   copyright notice rather than every notice the pool needs.
#' @param on_compare Optional callback taking one card code, invoked when
#'   the user asks to compare this card against everything it can meet.
#'   NULL omits the control entirely rather than showing one that does
#'   nothing, so a caller that has not mounted
#'   mod_matchup_explorer_server() does not advertise a view it cannot
#'   open. The caller, not this module, owns the transition: this module
#'   dismisses its own modal and hands the code over.
#' @export
mod_card_detail_server <- function(id, selected_code, cards, rulings = NULL,
                                   legality = NULL, on_compare = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(selected_code(), {
      shiny::req(selected_code())
      shiny::showModal(shiny::modalDialog(
        # An empty marker div identifying WHICH modal this is. There is
        # only ever one `#shiny-modal` element, and the dismissal handler
        # below is delegated on `document` rather than bound to the modal
        # itself, so it survives that element being replaced -- which
        # means it also fires for a DIFFERENT module's modal. With the
        # card detail modal the only one in the app that was harmless;
        # once mod_matchup_explorer_server() added a second, closing
        # either one cleared both modules' reactiveVals. The clearing
        # then landed in the middle of a detail -> matchup -> detail
        # transition and blanked the modal that had just opened, because
        # the R side sets the new value synchronously while this event
        # arrives a round trip later. Guarding on the marker keeps each
        # handler to its own modal.
        shiny::div(class = "dc-detail-modal", style = "display: none;"),
        shiny::uiOutput(session$ns("detail")),
        # Bootstrap's own `hidden.bs.modal` event fires on EVERY
        # dismissal path -- the Close button, the backdrop click, or
        # Escape -- so selected_code() is cleared uniformly below rather
        # than only on the Close button, which is what forced
        # easyClose = FALSE in an earlier draft (a click-outside/Escape
        # dismissal would close the modal visually while selected_code()
        # stayed set, so reopening the SAME card right after would no-op,
        # since the observer above only fires on a *change*). The
        # `.dcDetail`-namespaced `.off().on()` pair keeps this idempotent
        # across repeated opens, since this script tag re-runs every time
        # the modal reopens.
        shiny::tags$script(shiny::HTML(sprintf(
          "$(document).off('hidden.bs.modal.dcDetail').on('hidden.bs.modal.dcDetail', '#shiny-modal', function() {
            if (!$(this).find('.dc-detail-modal').length) { return; }
            Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
          });",
          session$ns("dismissed")
        ))),
        size = "l", easyClose = TRUE,
        footer = shiny::tagList(
          if (!is.null(on_compare)) {
            shiny::actionButton(session$ns("compare"), "Compare vs all")
          },
          shiny::actionButton(session$ns("close"), "Close")
        )
      ))
    })

    shiny::observeEvent(input$close, {
      shiny::removeModal()
    })

    # Registered only when there is something to call, matching the
    # footer above: with no on_compare the control is never rendered, so
    # an observer for it would be waiting on an input that cannot arrive.
    if (!is.null(on_compare)) {
      # The code is read BEFORE the modal is dismissed: removeModal()
      # leads to the handler above clearing selected_code(), so reading
      # it afterwards would hand the caller a NULL.
      shiny::observeEvent(input$compare, {
        shiny::req(selected_code())
        code <- selected_code()
        shiny::removeModal()
        on_compare(code)
      })
    }

    shiny::observeEvent(input$dismissed, {
      selected_code(NULL)
    }, ignoreInit = TRUE)

    output$detail <- shiny::renderUI({
      shiny::req(selected_code())
      card <- cards[cards$code == selected_code(), ]
      if (nrow(card) == 0) return(NULL)

      # matchup_overrides is pair-keyed (ice_code, breaker_code), not
      # single-card -- a card-detail view has no counterpart card to pair
      # against, so it has no `source` value to show. Provenance display
      # belongs to the matchup table only (see mod_matchup_explorer.R).
      #
      # This view renders cardpool data (title/text/image), so it must
      # call require_cardpool_disclaimer(CARDPOOL_DISCLAIMER_CONFIRMED)
      # AND actually render the
      # disclaimer text below -- the guard is a side-effecting assertion,
      # not a UI element, so it is called here rather than placed inside
      # tagList() as if it were a tag.
      require_cardpool_disclaimer(CARDPOOL_DISCLAIMER_CONFIRMED)

      # Middle dot as a \u escape, not the literal character: R CMD check
      # warns on any non-ASCII byte in R source outside comments.
      separator <- "\u00b7"

      shiny::tagList(
        shiny::div(class = "d-flex gap-3",
          shiny::tags$img(src = card_image_url(card$code),
                          style = "max-width: 300px;", loading = "lazy"),
          shiny::div(
            shiny::h4(card$title),
            shiny::p(shiny::strong(card$faction_code),
                     paste0(" ", separator, " "), card$type_code,
                     if (!is.na(card$keywords)) paste(separator, card$keywords)),
            shiny::tags$pre(card$text)
          )
        ),
        # This view shows ONE card, so it names one publisher.
        cardpool_disclaimer_ui(card_publishers(card$code, legality)),
        # Rulings, official FAQ answers and release-note errata for this
        # card. Returns NULL when the nrdb attribution gate is closed or
        # the card has none, so nothing here claims a card is unruled.
        card_rulings_ui(rulings, card$title)
      )
    })

    # Same reason as mod_card_browser_server(): this output is bound when
    # the session starts, but its element only exists while the modal is
    # open. Shiny suspends an output whose element it cannot see, and on
    # this path does not reliably resume it -- the modal then opens empty,
    # with the R process idle, which looks like a data problem and is not.
    # It rendered on the FIRST open and stopped after a close and reopen,
    # which is exactly how this hid until someone clicked twice.
    shiny::outputOptions(output, "detail", suspendWhenHidden = FALSE)
  })
}
