#' Pool browsing: faction/type/subtype/side filters, image grid, code-keyed.
#'
#' `card.title` is not used as an identity key anywhere below -- only
#' `card.code` (the schema's actual primary key) -- since titles are not
#' unique-safe across reprints. Exported for the same cross-namespace
#' reason as mod_card_detail_ui() (see its docs), and uses explicit
#' `shiny::` prefixes for the same reason given there.
#'
#' @param id Module id.
#' @export
mod_card_browser_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shinyWidgets::pickerInput(ns("side"), "Side", choices = c("corp", "runner"), multiple = TRUE),
      shinyWidgets::pickerInput(ns("faction"), "Faction", choices = NULL, multiple = TRUE, options = list(`live-search` = TRUE)),
      shinyWidgets::pickerInput(ns("type"), "Type", choices = NULL, multiple = TRUE),
      shinyWidgets::pickerInput(ns("subtype"), "Subtype", choices = NULL, multiple = TRUE, options = list(`live-search` = TRUE))
    ),
    shiny::mainPanel(
      shiny::tags$p(class = "text-muted small",
        "Not maintained, produced, endorsed, supported, or affiliated with Fantasy Flight Games and/or Wizards of the Coast."
      ),
      shiny::uiOutput(ns("card_grid"))
    )
  )
}

#' Card browser module server
#'
#' @param id Module id.
#' @param cards The active cardpool's `card` data frame.
#' @param selected_code The shared `reactiveVal` owned by app_server() and
#'   read by the single mod_card_detail_server() instance -- this module
#'   only ever calls `selected_code(code)`, never showModal()/
#'   moduleServer() itself.
#' @export
mod_card_browser_server <- function(id, cards, selected_code) {
  shiny::moduleServer(id, function(input, output, session) {

    require_cardpool_disclaimer(TRUE)

    shiny::observe({
      shinyWidgets::updatePickerInput(session, "faction", choices = sort(unique(cards$faction_code)))
      shinyWidgets::updatePickerInput(session, "type", choices = sort(unique(cards$type_code)))
      shinyWidgets::updatePickerInput(session, "subtype",
        choices = sort(unique(unlist(strsplit(stats::na.omit(cards$keywords), " - ")))))
    })

    filtered <- shiny::reactive({
      d <- cards
      if (length(input$side))    d <- d[d$side_code %in% input$side, ]
      if (length(input$faction)) d <- d[d$faction_code %in% input$faction, ]
      if (length(input$type))    d <- d[d$type_code %in% input$type, ]
      if (length(input$subtype)) d <- d[!is.na(d$keywords) & grepl(paste(input$subtype, collapse = "|"), d$keywords), ]
      d
    })

    output$card_grid <- shiny::renderUI({
      safe_render(function() {
        d <- filtered()
        if (nrow(d) == 0) {
          return(alert_box("No cards match the current filters.", "info"))
        }
        shiny::tagList(lapply(seq_len(nrow(d)), function(i) {
          code <- d$code[i]
          shiny::tags$img(
            src = card_image_url(code),
            style = "width: 150px; margin: 4px; cursor: pointer;",
            loading = "lazy",
            onclick = click_sets_input(session, "card_clicked", code)
          )
        }))
      })
    })

    # Single shared input, set via onclick JS above, rather than one
    # actionLink/observer per card -- keeps this from growing linearly
    # with pool size (400+ cards).
    shiny::observeEvent(input$card_clicked, {
      selected_code(input$card_clicked)
    })
  })
}
