# R/mod_deck_compare.R - Deck Compare view module.
#
# A nav destination, not a modal: it is entered with nothing selected and supplies its own
# inputs, so there is no opener to hang a modal from (DL-031). Instantiated once per
# session beside the other view modules - re-instantiating on navigation accumulates
# observers and silently drops view state.
#
# All deck state is session-scoped reactiveVal(). No dbWriteTable(), no store_root, no
# lineage entry (DL-037). Release-pinning transparency extends the existing release_info
# caption style rather than introducing a banner.

#' Deck Compare UI
#'
#' @param id Module namespace id.
#' @return A `shiny::tagList`.
#' @export
mod_deck_compare_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "dc-deck-compare",
      shiny::textInput(ns("corp_ref"), "Corp deck (id or NetrunnerDB URL)"),
      shiny::textInput(ns("runner_ref"), "Runner deck (id or NetrunnerDB URL)"),
      shiny::actionButton(ns("compare"), "Compare")
    ),
    shiny::uiOutput(ns("status")),
    shiny::uiOutput(ns("unresolved_caption")),
    reactable::reactableOutput(ns("results_table")),
    implementation_mit_notice_ui(),
    cardpool_disclaimer_ui()
  )
}

#' Deck Compare view module
#'
#' @param id Module namespace id.
#' @param all_codes,cards,matchup,cardpool_release_id App data from
#'   [load_ice_breaker_app_data()].
#' @return `mod_deck_compare_ui()` returns a `shiny::tagList`; the server returns
#'   invisibly and holds all deck state in session-scoped `reactiveVal()`s.
#' @details A top-level UI mount exists here, unlike `mod_card_detail_ui()` and the
#'   matchup explorer, because this is a nav destination swapped into the main slot rather
#'   than a modal building its own outputs (DL-031). Nothing is persisted: no
#'   `dbWriteTable()`, no `store_root`, no `.LINEAGE_REGISTRY` entry. Decks are discarded
#'   on disconnect (DL-037). A refused fetch or refused deck renders through
#'   `alert_box()` and leaves any prior result standing rather than blanking the view,
#'   because Compare is an explicit press against free-text references and a typo should
#'   not destroy the comparison being read (DL-040). Instantiate once per session; the
#'   module is not re-instantiated on navigation.
#' @export
mod_deck_compare_server <- function(id, all_codes, cards, matchup, cardpool_release_id) {
  shiny::moduleServer(id, function(input, output, session) {

    # This view renders both cardpool titles and implementation-derived
    # economics, so both attribution gates are asserted here, exactly as
    # mod_matchup_explorer_server() does for the single-card view.
    require_cardpool_disclaimer(CARDPOOL_DISCLAIMER_CONFIRMED)
    require_implementation_license_notice(IMPLEMENTATION_MIT_NOTICE_CONFIRMED)

    corp_deck <- shiny::reactiveVal(NULL)
    runner_deck <- shiny::reactiveVal(NULL)
    result <- shiny::reactiveVal(NULL)
    status_message <- shiny::reactiveVal(NULL)

    shiny::observeEvent(input$compare, {
      status_message(NULL)

      nrdb <- lineage("nrdb")

      corp_fetched <- tryCatch(fetch_deck(input$corp_ref, nrdb), error = function(e) e)
      if (inherits(corp_fetched, "error") || is.null(corp_fetched)) {
        status_message("Could not fetch the Corp deck. Check the reference and try again.")
        return(invisible(NULL))
      }

      runner_fetched <- tryCatch(fetch_deck(input$runner_ref, nrdb), error = function(e) e)
      if (inherits(runner_fetched, "error") || is.null(runner_fetched)) {
        status_message("Could not fetch the Runner deck. Check the reference and try again.")
        return(invisible(NULL))
      }

      corp_resolved <- tryCatch(resolve_deck_codes(corp_fetched, all_codes), error = function(e) e)
      if (inherits(corp_resolved, "error")) {
        status_message("The Corp deck could not be resolved: it has no cards or no identity.")
        return(invisible(NULL))
      }

      runner_resolved <- tryCatch(resolve_deck_codes(runner_fetched, all_codes), error = function(e) e)
      if (inherits(runner_resolved, "error")) {
        status_message("The Runner deck could not be resolved: it has no cards or no identity.")
        return(invisible(NULL))
      }

      # Every prior-fetch/resolve failure above returned before touching
      # corp_deck/runner_deck/result, so a refused compare leaves whatever
      # was on screen exactly as it was (DL-040) - the reactiveVals below
      # are only ever set once this point is reached with two valid decks.
      corp_deck(list(deck = corp_fetched, resolved = corp_resolved))
      runner_deck(list(deck = runner_fetched, resolved = runner_resolved))

      pairing <- deck_matchups(
        matchup, corp_resolved$ice, runner_resolved$breakers, corp_fetched$cards
      )
      result(list(
        pairing = pairing,
        corp_unresolved = corp_resolved$unresolved,
        runner_unresolved = runner_resolved$unresolved
      ))
    })

    output$status <- shiny::renderUI({
      safe_render(function() {
        if (is.null(status_message())) return(NULL)
        alert_box(status_message(), "warning")
      })
    })

    output$unresolved_caption <- shiny::renderUI({
      safe_render(function() {
        r <- result()
        if (is.null(r)) return(NULL)
        unresolved_codes_caption(
          c(r$corp_unresolved, r$runner_unresolved), cardpool_release_id
        )
      })
    })

    output$results_table <- reactable::renderReactable({
      safe_render(function() {
        r <- result()
        if (is.null(r)) return(NULL)

        rows <- matchup_display(r$pairing$matchups, NULL, cards)
        if (nrow(rows) == 0) return(NULL)

        copy_counts <- r$pairing$copy_counts
        rows$copy_count <- unname(copy_counts[rows$ice_code])

        rows <- rows[, c("ice_title", "copy_count", "breaker_title", "cost_to_break",
                         "credit_differential", "source"), drop = FALSE]

        column_defs <- c(
          list(
            ice_title = reactable::colDef(name = "Ice"),
            copy_count = reactable::colDef(name = "Copies", aggregate = "unique"),
            breaker_title = reactable::colDef(name = "Breaker")
          ),
          matchup_value_coldefs()
        )

        reactable::reactable(
          rows, columns = column_defs, groupBy = "ice_title",
          defaultSorted = "ice_title"
        )
      })
    })

    shiny::outputOptions(output, "status", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "unresolved_caption", suspendWhenHidden = FALSE)
    shiny::outputOptions(output, "results_table", suspendWhenHidden = FALSE)

    invisible(NULL)
  })
}

#' Caption naming every code the active cardpool release does not know
#'
#' @param unresolved_codes Character vector of codes absent from `all_codes` entirely.
#' @param cardpool_release_id Character. The active cardpool release directory name.
#' @return A `shiny::tags$div`, or `NULL` when there is nothing unresolved.
#' @details Draws no distinction between a code newer than the pinned release and a
#'   reprint alias: `cardpool.sql` carries no previous-versions or reprint-alias column,
#'   so that split would be inference presented as measurement (DL-032). A code that IS
#'   known but neither ice nor a breaker (`other_known` from [resolve_deck_codes()]) is
#'   absent from this caption entirely - an Agenda not appearing in an ice-versus-breaker
#'   table is the table working correctly, not a gap.
#' @keywords internal
unresolved_codes_caption <- function(unresolved_codes, cardpool_release_id) {
  unresolved_codes <- unique(unresolved_codes)
  if (length(unresolved_codes) == 0) return(NULL)

  shiny::tags$div(
    style = "font-size: 11px; color: #8a8f98;",
    sprintf(
      "not in cardpool release %s: %s",
      cardpool_release_id, paste(unresolved_codes, collapse = ", ")
    )
  )
}
