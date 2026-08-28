#' Shared options for the browser's multi-select pickers
#'
#' `actions-box` is what makes a chosen filter removable: a
#' shinyWidgets multi-picker without it has no deselect-all control, so
#' a user who picks one faction can only swap it for another and the
#' filter appears permanent once set.
#' @keywords internal
MULTI_PICKER_OPTIONS <- list(`actions-box` = TRUE, `none-selected-text` = "Any")

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
      # actions-box renders Select All and Deselect All as a pair, but
      # "select every type" is identical to "filter by no type" -- a
      # control that looks like it does something and does nothing. Only
      # the deselect half is meaningful, so the other is hidden. The rule
      # itself is in inst/shiny-app/www/netrunner.css with the rest of
      # the theme, rather than as an inline <style> only this module
      # knows about.
      shiny::textInput(ns("query"), "Search",
        placeholder = "e.g. t:ice cost<4 s:barrier"),
      # The parsed query echoed back in words, via search_explain(): a
      # typo that returns nothing then reads as "faction is 'hb'" rather
      # than as an empty grid with no explanation.
      shiny::uiOutput(ns("query_feedback")),
      shinyWidgets::pickerInput(ns("format"), "Format", choices = NULL),
      # `actions-box` adds the Select All / Deselect All controls. Without
      # it a multi-select picker offers no way back to an empty selection
      # once anything is chosen -- you can only swap one value for
      # another, which reads as "this filter cannot be turned off".
      # `none-selected-text` then says what an empty selection MEANS,
      # since a blank box and "no filter applied" look identical.
      shinyWidgets::pickerInput(ns("side"), "Side", choices = c("corp", "runner"),
        multiple = TRUE, options = MULTI_PICKER_OPTIONS),
      shinyWidgets::pickerInput(ns("faction"), "Faction", choices = NULL,
        multiple = TRUE, options = c(MULTI_PICKER_OPTIONS, list(`live-search` = TRUE))),
      shinyWidgets::pickerInput(ns("type"), "Type", choices = NULL,
        multiple = TRUE, options = MULTI_PICKER_OPTIONS),
      shinyWidgets::pickerInput(ns("subtype"), "Subtype", choices = NULL,
        multiple = TRUE, options = c(MULTI_PICKER_OPTIONS, list(`live-search` = TRUE))),
      shiny::actionButton(ns("clear"), "Clear all filters", class = "btn-sm btn-outline-secondary")
    ),
    shiny::mainPanel(
      cardpool_disclaimer_ui(),
      shiny::uiOutput(ns("card_grid"))
    )
  )
}

#' The search registry the browser uses
#'
#' [cardpool_search_fields()] plus [legality_search_fields()], so
#' `is_banned:true` and `points>1` work in the same query as `t:ice`.
#' The legality operands only resolve against cards that have been
#' through [annotate_format_legality()], which is why this is assembled
#' here rather than folded into cardpool_search_fields() itself -- that
#' registry describes the raw `card` table, which has no such columns.
#' @return A field registry.
#' @export
browser_search_fields <- function() {
  do.call(search_field_registry, c(
    as.list(cardpool_search_field_specs()),
    as.list(legality_search_fields()),
    list(default_field = "title")
  ))
}

#' Card browser module server
#'
#' @param id Module id.
#' @param cards The active cardpool's `card` data frame.
#' @param selected_code The shared `reactiveVal` owned by app_server() and
#'   read by the single mod_card_detail_server() instance -- this module
#'   only ever calls `selected_code(code)`, never showModal()/
#'   moduleServer() itself.
#' @param legality The `legality` element of
#'   [load_ice_breaker_app_data()]: the card-pool and ban-list tables, any
#'   of which may be NULL against a release promoted before that schema
#'   existed. NULL disables the format selector entirely and leaves every
#'   card in pool, which is the honest rendering of "this release carries
#'   no legality data" rather than silently showing an unfiltered pool as
#'   though a format had been applied.
#' @export
mod_card_browser_server <- function(id, cards, selected_code, legality = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    require_cardpool_disclaimer(CARDPOOL_DISCLAIMER_CONFIRMED)

    has_legality <- !is.null(legality) &&
      !is.null(legality$format_snapshot) && nrow(legality$format_snapshot) > 0

    shiny::observe({
      shinyWidgets::updatePickerInput(session, "faction", choices = sort(unique(cards$faction_code)))
      shinyWidgets::updatePickerInput(session, "type", choices = sort(unique(cards$type_code)))
      shinyWidgets::updatePickerInput(session, "subtype",
        choices = sort(unique(unlist(strsplit(stats::na.omit(cards$keywords), " - ")))))

      # "Any format" is a real choice, not a placeholder: it is the only
      # way to see cards from every pool at once, which is what the
      # browser did before a format selector existed.
      format_choices <- c("Any format" = "")
      if (has_legality && !is.null(legality$format)) {
        named <- stats::setNames(legality$format$id, legality$format$name)
        format_choices <- c(format_choices, named)
      }
      shinyWidgets::updatePickerInput(session, "format", choices = format_choices,
                                      selected = if (has_legality) "standard" else "")
    })

    # Legality columns, recomputed only when the chosen format changes --
    # not per keystroke in the search box.
    annotated <- shiny::reactive({
      # A NULL snapshot short-circuits before the table arguments are
      # touched, so passing NULL for all of them is correct rather than
      # merely convenient -- see annotate_format_legality().
      if (!has_legality || !nzchar(input$format %||% "")) {
        return(annotate_format_legality(cards, NULL, NULL, NULL, NULL, NULL))
      }
      snapshot <- active_snapshot(legality$format_snapshot, input$format)
      annotate_format_legality(
        cards, snapshot, legality$card_pool_set, legality$restriction_card,
        legality$printing, legality$card_set
      )
    })

    # The query parsed once per change, kept separate from the grid so a
    # parse error can be reported without also blanking the results.
    parsed <- shiny::reactive({
      query <- input$query %||% ""
      if (!nzchar(trimws(query))) return(list(ok = TRUE, ast = NULL))
      tryCatch(
        list(ok = TRUE, ast = search_parse(query, browser_search_fields())),
        netrunneR_search_unknown_field = function(e) list(ok = FALSE, message = conditionMessage(e)),
        netrunneR_search_parse_error = function(e) list(ok = FALSE, message = conditionMessage(e)),
        netrunneR_search_bad_operator = function(e) list(ok = FALSE, message = conditionMessage(e)),
        netrunneR_search_bad_value = function(e) list(ok = FALSE, message = conditionMessage(e))
      )
    })

    output$query_feedback <- shiny::renderUI({
      p <- parsed()
      if (!isTRUE(p$ok)) return(alert_box(p$message, "warning"))
      if (is.null(p$ast)) return(NULL)
      shiny::tags$p(class = "text-muted small",
                    sprintf("Reading as: %s", search_explain(p$ast)))
    })

    filtered <- shiny::reactive({
      d <- annotated()
      if (length(input$side))    d <- d[d$side_code %in% input$side, ]
      if (length(input$faction)) d <- d[d$faction_code %in% input$faction, ]
      if (length(input$type))    d <- d[d$type_code %in% input$type, ]
      if (length(input$subtype)) d <- d[!is.na(d$keywords) & grepl(paste(input$subtype, collapse = "|"), d$keywords), ]

      p <- parsed()
      # A query that does not parse filters nothing: the message above
      # already says why, and silently emptying the grid would read as
      # "no cards match" instead of "that query is malformed".
      if (isTRUE(p$ok) && !is.null(p$ast)) d <- d[search_match(p$ast, d), , drop = FALSE]
      d
    })

    output$card_grid <- shiny::renderUI({
      safe_render(function() {
        d <- filtered()
        if (nrow(d) == 0) {
          return(alert_box("No cards match the current filters.", "info"))
        }

        in_pool <- d[d$in_rotation, , drop = FALSE]
        out_of_pool <- d[!d$in_rotation, , drop = FALSE]

        shiny::tagList(
          card_grid_tags(session, in_pool),
          # Excluded cards collapse rather than vanish, so a card that is
          # simply out of format is distinguishable from one that does
          # not exist.
          if (nrow(out_of_pool)) {
            shiny::tags$details(
              shiny::tags$summary(class = "text-muted",
                sprintf("%d hidden by card pool", nrow(out_of_pool))),
              card_grid_tags(session, out_of_pool)
            )
          }
        )
      })
    })

    # One control that empties every filter at once. Clearing them
    # individually means five separate deselect-alls plus emptying the
    # search box, which is enough friction that a user is more likely to
    # reload the page -- losing the selected card and any scroll position
    # with it.
    #
    # Format resets to "Any format", not to Standard: "clear all filters"
    # that leaves a format applied is not clearing all filters. Standard
    # remains the default on FIRST load, which is a different question.
    shiny::observeEvent(input$clear, {
      shiny::updateTextInput(session, "query", value = "")
      shinyWidgets::updatePickerInput(session, "format", selected = "")
      for (filter_id in c("side", "faction", "type", "subtype")) {
        shinyWidgets::updatePickerInput(session, filter_id, selected = character(0))
      }
    })

    # Single shared input, set via onclick JS above, rather than one
    # actionLink/observer per card -- keeps this from growing linearly
    # with pool size (400+ cards).
    shiny::observeEvent(input$card_clicked, {
      selected_code(input$card_clicked)
    })
  })
}

#' Render one grid of card images
#'
#' Split out because the browser now renders two grids -- in pool and
#' hidden-by-card-pool -- and they must stay identical in click wiring
#' and lazy loading.
#'
#' A banned card is dimmed and titled with the reason rather than
#' dropped, matching annotate_format_legality()'s own choice to annotate
#' instead of filter.
#' @param session The module session, for click_sets_input().
#' @param d A data frame of cards, already annotated.
#' @return A shiny tagList.
#' @keywords internal
card_grid_tags <- function(session, d) {
  shiny::tags$div(class = "nr-grid", lapply(seq_len(nrow(d)), function(i) {
    code <- d$code[i]
    banned <- isTRUE(d$is_banned[i])
    restricted <- isTRUE(d$is_restricted[i])
    note <- if (banned) "Banned in this format" else if (restricted) "Restricted in this format" else NULL
    shiny::tags$img(
      src = card_image_url(code),
      title = note,
      # Classes, not inline style: the grid is themed in
      # inst/shiny-app/www/netrunner.css, and a size or a dimming rule
      # that only exists here cannot be changed with the rest of the
      # visual identity.
      class = paste("nr-card", if (banned) "nr-card--banned"),
      loading = "lazy",
      onclick = click_sets_input(session, "card_clicked", code)
    )
  }))
}
