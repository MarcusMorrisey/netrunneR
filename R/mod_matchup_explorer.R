#' Arbitrary ice-vs-breaker comparison over compute_ice_breaker_matchups()'s
#' output. `cards` supplies display titles by code; `matchup` is the
#' tibble compute_ice_breaker_matchups() returns -- cost_to_break /
#' credit_differential / source already computed, never re-derived here.
#' This view renders BOTH cardpool data (card titles) and
#' implementation-derived data (cost_to_break/credit_differential, which
#' derive from ice_breaker_traits) -- see mod_matchup_explorer_server()
#' for both required attribution calls. Uses explicit `shiny::` prefixes
#' for the reason given on mod_card_detail_ui().
#'
#' @param id Module id.
#' @export
mod_matchup_explorer_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::tags$p(class = "text-muted small",
      "Card data: not maintained, produced, endorsed, supported, or affiliated with Fantasy Flight Games and/or Wizards of the Coast. ",
      "Ice/breaker interaction logic derived from the mtgred/netrunner implementation (MIT licensed)."
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::radioButtons(ns("mode"), "Compare",
               choices = c("One breaker vs all ice" = "breaker_vs_all",
                           "One ice vs all breakers" = "ice_vs_all",
                           "All vs all" = "all"))),
      shiny::column(4, shiny::selectInput(ns("breaker_code"), "Breaker", choices = NULL)),
      shiny::column(4, shiny::selectInput(ns("ice_code"), "Ice", choices = NULL))
    ),
    # A separate slot for the empty-selection message: reactableOutput()
    # is htmlwidget-typed, and an earlier draft tried to return a plain
    # alert_box() shiny.tag from inside renderReactable() for this case --
    # renderReactable() has no way to treat a non-widget value as
    # something to display, so it silently produced an empty widget
    # ("x": null) instead of showing the message.
    shiny::uiOutput(ns("matchup_status")),
    reactable::reactableOutput(ns("matchup_table"))
  )
}

#' Matchup explorer module server
#'
#' @param id Module id.
#' @param cards The active cardpool's `card` data frame.
#' @param matchup The tibble compute_ice_breaker_matchups() returns.
#' @param selected_code Same shared reactiveVal as mod_card_browser_server()
#'   receives -- both callers set it; only mod_card_detail_server() reads it.
#' @export
mod_matchup_explorer_server <- function(id, cards, matchup, selected_code) {
  shiny::moduleServer(id, function(input, output, session) {

    require_cardpool_disclaimer(TRUE)
    require_implementation_license_notice(TRUE)

    breakers <- cards[cards$type_code == "program" & cards$side_code == "runner", ]
    ice      <- cards[cards$type_code == "ice", ]

    shiny::observe({
      shiny::updateSelectInput(session, "breaker_code",
        choices = stats::setNames(breakers$code, breakers$title))
      shiny::updateSelectInput(session, "ice_code",
        choices = stats::setNames(ice$code, ice$title))
    })

    result <- shiny::reactive({
      switch(input$mode,
        breaker_vs_all = matchup[matchup$breaker_code == input$breaker_code, ],
        ice_vs_all     = matchup[matchup$ice_code == input$ice_code, ],
        all            = matchup
      )
    })

    display <- shiny::reactive({
      d <- result()
      if (nrow(d) == 0) return(d)
      d$ice_title     <- cards$title[match(d$ice_code, cards$code)]
      d$breaker_title <- cards$title[match(d$breaker_code, cards$code)]
      d[order(d$cost_to_break, na.last = TRUE), ]
    })

    output$matchup_status <- shiny::renderUI({
      safe_render(function() {
        if (nrow(display()) == 0) alert_box("No matchups for the current selection.", "info") else NULL
      })
    })

    output$matchup_table <- reactable::renderReactable({
      safe_render(function() {
        d <- display()
        if (nrow(d) == 0) return(NULL)

        # A row represents a PAIR, so clicking the row itself is
        # ambiguous about which card to open; clicking the specific
        # ice/breaker name resolves that.
        make_click_cell <- function(code_column) {
          function(value, index) {
            code <- d[[code_column]][index]
            htmltools::tags$span(
              value, style = "cursor: pointer; text-decoration: underline;",
              onclick = click_sets_input(session, "row_card_clicked", code)
            )
          }
        }

        reactable::reactable(
          d,
          columns = list(
            ice_code = reactable::colDef(show = FALSE),
            breaker_code = reactable::colDef(show = FALSE),
            ice_title = reactable::colDef(name = "Ice", cell = make_click_cell("ice_code")),
            breaker_title = reactable::colDef(name = "Breaker", cell = make_click_cell("breaker_code")),
            source = reactable::colDef(name = "Source",
              cell = function(value) if (value == "not_computable") "not yet computable" else value),
            credit_differential = reactable::colDef(
              name = "Credit differential (rez - break; + favors runner)"
            )
          ),
          defaultSorted = "cost_to_break"
        )
      })
    })

    shiny::observeEvent(input$row_card_clicked, {
      selected_code(input$row_card_clicked)
    })
  })
}
