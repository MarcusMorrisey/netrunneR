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
#' @export
mod_card_detail_server <- function(id, selected_code, cards) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(selected_code(), {
      shiny::req(selected_code())
      shiny::showModal(shiny::modalDialog(
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
            Shiny.setInputValue('%s', Math.random(), {priority: 'event'});
          });",
          session$ns("dismissed")
        ))),
        size = "l", easyClose = TRUE,
        footer = shiny::actionButton(session$ns("close"), "Close")
      ))
    })

    shiny::observeEvent(input$close, {
      shiny::removeModal()
    })

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
      # call require_cardpool_disclaimer(TRUE) AND actually render the
      # disclaimer text below -- the guard is a side-effecting assertion,
      # not a UI element, so it is called here rather than placed inside
      # tagList() as if it were a tag.
      require_cardpool_disclaimer(TRUE)

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
        shiny::tags$p(class = "text-muted small",
          "Not maintained, produced, endorsed, supported, or affiliated with Fantasy Flight Games and/or Wizards of the Coast."
        )
      )
    })
  })
}
