#' Shared options for the browser's multi-select pickers
#'
#' `actions-box` is what makes a chosen filter removable: a
#' shinyWidgets multi-picker without it has no deselect-all control, so
#' a user who picks one faction can only swap it for another and the
#' filter appears permanent once set.
#' @keywords internal
MULTI_PICKER_OPTIONS <- list(`actions-box` = TRUE, `none-selected-text` = "Any")

#' Pool browsing: side/faction/subtype filters, image grid, code-keyed.
#'
#' `card.title` is not used as an identity key anywhere below -- only
#' `card.code` (the schema's actual primary key) -- since titles are not
#' unique-safe across reprints. Exported for the same cross-namespace
#' reason as mod_card_detail_ui() (see its docs), and uses explicit
#' `shiny::` prefixes for the same reason given there.
#'
#' @param id Module id.
#' @param side Character. Which side the Side control starts on:
#'   `"corp"` (the browsing default), `"runner"`, or `""` for Any.
#'   `NULL` omits the control altogether, for a caller that has already
#'   narrowed the pool to one side -- there the control cannot do
#'   anything except contradict the data it was handed, since the only
#'   states it offers are "the side you already have" and "nothing".
#'   `filtered()` reads `input$side %||% ""`, so an absent control is
#'   already the Any state and the server needs no matching change.
#' @param choices The value [browser_choices()] returns, or NULL. These
#'   are STATIC for a given pool -- the factions, subtypes and formats
#'   present in it are known before any session -- so they are built into
#'   the UI rather than pushed from the server after the fact.
#'
#'   That is not a style preference. The server used to send them with
#'   updatePickerInput() from an observe() at session start, which worked
#'   while this module was a tab that existed in the DOM from the
#'   beginning. Mounted inside a modal it is not: the element does not
#'   exist until someone opens the picker, the update message has nothing
#'   to land on, and it is dropped. Every list came up empty and Format
#'   lost its Standard default -- a picker with no choices renders as a
#'   lone search box that matches nothing, which looks like a broken
#'   filter rather than an unpopulated one.
#'
#'   NULL renders empty pickers, which is only useful to a caller that
#'   drives the inputs directly, such as a test.
#' @export
mod_card_browser_ui <- function(id, side = "corp", choices = NULL) {
  stopifnot(is.null(side) || (is.character(side) && length(side) == 1 &&
                              side %in% c("corp", "runner", "")))
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
      shinyWidgets::pickerInput(ns("format"), "Format",
        choices = choices$format %||% character(0),
        selected = choices$format_selected %||% ""),
      # `actions-box` adds the Select All / Deselect All controls. Without
      # it a multi-select picker offers no way back to an empty selection
      # once anything is chosen -- you can only swap one value for
      # another, which reads as "this filter cannot be turned off".
      # `none-selected-text` then says what an empty selection MEANS,
      # since a blank box and "no filter applied" look identical.
      # Side is exclusive, not multi-select: only Corps have ICE and only
      # Runners have programs, so the two pools do not overlap and there
      # is nothing to be gained from holding both open at once. Picking
      # one switches the other off, which radio semantics give for free.
      #
      # "Any" is kept even so, because the pool is browsable as a whole
      # and losing that would be a capability change, not a tidy-up.
      # Corp is the default on first load; Clear all filters returns to
      # Any, matching the Format convention below -- a default and a
      # cleared state are different questions.
      if (!is.null(side)) shinyWidgets::radioGroupButtons(
        ns("side"), "Side",
        choices = c("Any" = "", "Corp" = "corp", "Runner" = "runner"),
        selected = side, justified = TRUE, size = "sm"
      ),
      shinyWidgets::pickerInput(ns("faction"), "Faction",
        choices = choices$faction %||% character(0),
        multiple = TRUE, options = c(MULTI_PICKER_OPTIONS, list(`live-search` = TRUE))),
      # No Type picker in this version. Its only two values here are ice
      # and program, which is the Corp/Runner split restated -- the same
      # filter wearing a second name. The field stays searchable from the
      # query box (`t:ice`), so this removes a control, not a capability,
      # and the picker comes back when the pool widens to card types that
      # do not track side.
      shinyWidgets::pickerInput(ns("subtype"), "Subtype",
        choices = choices$subtype %||% character(0),
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
      # nzchar(), not length(): the radio always has a value, and "" is
      # the Any state rather than an absent input.
      side <- input$side %||% ""
      if (nzchar(side))          d <- d[d$side_code == side, ]
      if (length(input$faction)) d <- d[d$faction_code %in% input$faction, ]
      # has_card_subtype() (R/views-matchups.R), not grepl(): keywords is
      # a " - "-delimited string, so a substring match makes a subtype
      # that is a prefix of another silently pull the other one in.
      # Against the live pool that is two real collisions -- picking
      # "Corp" also returned Corporation cards, picking "Security" also
      # returned Security Protocol -- while the query box's own
      # `s:Security` tokenises correctly. Two paths for one question
      # should not give two answers.
      #
      # It also stopped building a regex out of user-selected values.
      # Nothing in the current pool carries a metacharacter, so that was
      # latent rather than live, but the picker's choices are upstream
      # data and nothing guarantees they stay that way.
      if (length(input$subtype)) {
        keep <- Reduce(`|`, lapply(input$subtype, function(st) has_card_subtype(d$keywords, st)))
        d <- d[keep, , drop = FALSE]
      }

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
      # Any, not back to the Corp default: a clear that leaves a side
      # applied has not cleared all filters, the same reasoning the
      # format reset above already follows.
      shinyWidgets::updateRadioGroupButtons(session, "side", selected = "")
      for (filter_id in c("faction", "subtype")) {
        shinyWidgets::updatePickerInput(session, filter_id, selected = character(0))
      }
    })

    # Single shared input, set via onclick JS above, rather than one
    # actionLink/observer per card -- keeps this from growing linearly
    # with pool size (400+ cards).
    shiny::observeEvent(input$card_clicked, {
      selected_code(input$card_clicked)
    })

    # This module's outputs are bound when the session starts, but
    # app_server() mounts it inside a modal, so their elements do not
    # exist in the DOM until someone opens the picker. Shiny suspends an
    # output whose element it cannot see and, on this path, does not
    # reliably resume it when the element finally appears -- the grid
    # then sits in `recalculating` forever, with the R process idle,
    # which reads exactly like a slow query and is not one.
    #
    # Computing unconditionally is cheap here: the whole grid is ~0.1s
    # and ~90KB of markup for the largest pool (390 ice), built once and
    # only recomputed when a filter changes.
    for (output_id in c("card_grid", "query_feedback")) {
      shiny::outputOptions(output, output_id, suspendWhenHidden = FALSE)
    }
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
      # The intrinsic size of every NetrunnerDB v2/large image, so the
      # browser can reserve each tile's box BEFORE the bytes arrive.
      # Without it an unloaded <img> is 150px wide and zero tall (CSS
      # gives .nr-card a width and no height, and there is no ratio to
      # infer yet), so the whole grid collapsed to a few pixels, every
      # tile counted as on-screen, and loading = "lazy" below dutifully
      # fetched all 390 ice at once -- roughly ten seconds before the
      # picker painted. These attributes only supply the ratio; CSS
      # still decides the rendered size (see .nr-card).
      width = 300,
      height = 418,
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

#' The static choices for one pool's filter pickers
#'
#' Factions, subtypes and formats are properties of the pool, not of a
#' session: they cannot change while the app is running, because the
#' active release cannot. Computing them once and building them into the
#' UI is both cheaper than a per-session round trip and, more to the
#' point, immune to the ordering problem that broke every filter when
#' this module moved into a modal -- see mod_card_browser_ui().
#'
#' "Any format" is a real choice, not a placeholder: it is the only way
#' to see cards from every pool at once, which is what the browser did
#' before a format selector existed. Standard is the DEFAULT when
#' legality data is available, because a card list that silently includes
#' rotated and banned cards is the more surprising of the two.
#'
#' @param cards A cardpool `card` data frame.
#' @param legality The legality tables, or NULL.
#' @return A list with `faction`, `subtype`, `format` and
#'   `format_selected`.
#' @export
browser_choices <- function(cards, legality = NULL) {
  has_legality <- !is.null(legality) &&
    !is.null(legality$format_snapshot) && nrow(legality$format_snapshot) > 0

  format_choices <- c("Any format" = "")
  if (has_legality && !is.null(legality$format)) {
    format_choices <- c(format_choices,
                        stats::setNames(legality$format$id, legality$format$name))
  }

  list(
    faction = sort(unique(cards$faction_code)),
    subtype = sort(unique(unlist(strsplit(stats::na.omit(cards$keywords), " - ")))),
    format = format_choices,
    format_selected = if (has_legality) "standard" else ""
  )
}
