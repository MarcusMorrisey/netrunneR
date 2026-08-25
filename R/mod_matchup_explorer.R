#' Arbitrary ice-vs-breaker comparison over compute_ice_breaker_matchups()'s
#' output. `cards` supplies display titles by code; `matchup` is the
#' tibble compute_ice_breaker_matchups() returns -- cost_to_break /
#' credit_differential / source already computed, never re-derived here.
#' This view renders BOTH cardpool data (card titles) and
#' implementation-derived data (cost_to_break/credit_differential, which
#' derive from ice_breaker_traits) -- see mod_matchup_explorer_server()
#' for both required attribution calls.
#' @export
mod_matchup_explorer_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$p(class = "text-muted small",
      "Card data: not maintained, produced, endorsed, supported, or affiliated with Fantasy Flight Games and/or Wizards of the Coast. ",
      "Ice/breaker interaction logic derived from the mtgred/netrunner implementation (MIT licensed)."
    ),
    fluidRow(
      column(4, radioButtons(ns("mode"), "Compare",
               choices = c("One breaker vs all ice" = "breaker_vs_all",
                           "One ice vs all breakers" = "ice_vs_all",
                           "All vs all" = "all"))),
      column(4, selectInput(ns("breaker_code"), "Breaker", choices = NULL)),
      column(4, selectInput(ns("ice_code"), "Ice", choices = NULL))
    ),
    reactable::reactableOutput(ns("matchup_table"))
  )
}

#' @param selected_code Same shared reactiveVal as mod_card_browser_server()
#'   receives -- both callers set it; only mod_card_detail_server() reads it.
#' @export
mod_matchup_explorer_server <- function(id, cards, matchup, selected_code) {
  moduleServer(id, function(input, output, session) {

    require_cardpool_disclaimer(TRUE)
    require_implementation_license_notice(TRUE)

    breakers <- cards[cards$type_code == "program" & cards$side_code == "runner", ]
    ice      <- cards[cards$type_code == "ice", ]

    observe({
      updateSelectInput(session, "breaker_code",
        choices = stats::setNames(breakers$code, breakers$title))
      updateSelectInput(session, "ice_code",
        choices = stats::setNames(ice$code, ice$title))
    })

    result <- reactive({
      switch(input$mode,
        breaker_vs_all = matchup[matchup$breaker_code == input$breaker_code, ],
        ice_vs_all     = matchup[matchup$ice_code == input$ice_code, ],
        all            = matchup
      )
    })

    display <- reactive({
      d <- result()
      if (nrow(d) == 0) return(d)
      d$ice_title     <- cards$title[match(d$ice_code, cards$code)]
      d$breaker_title <- cards$title[match(d$breaker_code, cards$code)]
      d[order(d$cost_to_break, na.last = TRUE), ]
    })

    output$matchup_table <- reactable::renderReactable({
      safe_render(function() {
        d <- display()
        if (nrow(d) == 0) {
          return(tags$div(class = "alert alert-info", "No matchups for the current selection."))
        }

        # A row represents a PAIR, so clicking the row itself is
        # ambiguous about which card to open; clicking the specific
        # ice/breaker name resolves that.
        make_click_cell <- function(code_column) {
          function(value, index) {
            code <- d[[code_column]][index]
            htmltools::tags$span(
              value, style = "cursor: pointer; text-decoration: underline;",
              onclick = sprintf(
                "Shiny.setInputValue('%s', '%s', {priority: 'event'})",
                session$ns("row_card_clicked"), code
              )
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

    observeEvent(input$row_card_clicked, {
      selected_code(input$row_card_clicked)
    })
  })
}
