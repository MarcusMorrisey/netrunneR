#' Shared component: image + stats + text for one card, invoked as a modal.
#'
#' Exported because `inst/shiny-app/app.R` calls this from outside the
#' package namespace -- `inst/` files are sourced by
#' `shiny::shinyAppDir()` separately from the package's own namespace, so
#' anything they call must be reachable via `netrunneR::`, not a bare
#' name.
#' @export
mod_card_detail_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("detail"))
}

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
  moduleServer(id, function(input, output, session) {
    observeEvent(selected_code(), {
      req(selected_code())
      # easyClose = FALSE deliberately: base Shiny has no server-side
      # event for a click-outside/Escape dismissal, so an easyClose modal
      # could close visually while selected_code() stays set -- reopening
      # the SAME card right after would then no-op, since the observer
      # above only fires on a *change*. The Close button below is the
      # only dismissal path, and it clears selected_code() explicitly.
      showModal(modalDialog(
        uiOutput(session$ns("detail")),
        size = "l", easyClose = FALSE,
        footer = actionButton(session$ns("close"), "Close")
      ))
    })

    observeEvent(input$close, {
      removeModal()
      selected_code(NULL)
    })

    output$detail <- renderUI({
      req(selected_code())
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

      tagList(
        div(class = "d-flex gap-3",
          tags$img(src = card_image_url(card$code),
                   style = "max-width: 300px;", loading = "lazy"),
          div(
            h4(card$title),
            p(strong(card$faction_code), " · ", card$type_code,
              if (!is.na(card$keywords)) paste("·", card$keywords)),
            tags$pre(card$text)
          )
        ),
        tags$p(class = "text-muted small",
          "Not maintained, produced, endorsed, supported, or affiliated with Fantasy Flight Games and/or Wizards of the Coast."
        )
      )
    })
  })
}
