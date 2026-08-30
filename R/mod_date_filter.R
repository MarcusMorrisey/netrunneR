#' Add a parsed date column, once
#'
#' abr writes dates as "2026.08.27." and parse_abr_date() is not free.
#' Both meta views filter on date, and re-parsing 4,400 of those on every
#' slider drag is work with no answer attached to it -- so it happens
#' once, when the module is created, and everything downstream reads the
#' column.
#'
#' @param tournaments The abr `tournament` table, or NULL.
#' @return `tournaments` with a `.date` column, or NULL.
#' @keywords internal
with_parsed_dates <- function(tournaments) {
  if (is.null(tournaments) || !nrow(tournaments)) return(NULL)
  tournaments$.date <- parse_abr_date(tournaments$date)
  tournaments
}

#' The span of dates present in the data
#'
#' @param dated Output of with_parsed_dates().
#' @return A length-2 Date vector, or NULL when nothing parsed.
#' @keywords internal
date_bounds <- function(dated) {
  if (is.null(dated)) return(NULL)
  d <- dated$.date[!is.na(dated$.date)]
  if (!length(d)) NULL else range(d)
}

#' The date filter, as a module both meta views mount
#'
#' A slider whose two endpoints both move, plus named shortcuts for the
#' rotation periods. It was written inline in the tournament map first;
#' it lives here because the stats view carries the same filter, and "the
#' same filter" is a claim two copies of the code cannot make honestly --
#' they can only drift.
#'
#' @param id Module id.
#' @export
mod_date_filter_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "filter"))
}

#' Date filter module server
#'
#' @param id Module id.
#' @param bounds A length-2 Date vector from date_bounds(), or NULL. NULL
#'   renders nothing rather than a slider over an invented range.
#' @param periods A data frame from rotation_periods(). Zero rows drops
#'   the shortcuts and leaves the slider, rather than inventing rotation
#'   dates -- there are seven and they are not guessable.
#' @return A reactive returning a length-2 Date vector: the selected
#'   range, falling back to the full bounds before the slider has
#'   rendered. NULL when there are no bounds at all.
#' @export
mod_date_filter_server <- function(id, bounds, periods) {
  shiny::moduleServer(id, function(input, output, session) {

    output$filter <- shiny::renderUI({
      safe_render(function() {
        if (is.null(bounds)) return(NULL)
        shiny::div(
          class = "nr-map-filter",
          shiny::sliderInput(
            session$ns("dates"), NULL,
            min = bounds[[1]], max = bounds[[2]],
            value = bounds, timeFormat = "%b %Y", width = "100%"
          ),
          # Presets MOVE THE SLIDER rather than replacing it. The slider
          # stays the single source of truth for what is shown, so a
          # preset is a shortcut to a range and never a second, competing
          # filter whose disagreement with the slider a reader would have
          # to resolve.
          if (nrow(periods)) {
            shiny::div(
              class = "nr-map-presets",
              shiny::tags$span(class = "nr-preset-label", "Jump to:"),
              shiny::actionLink(session$ns("preset_all"), "All time",
                                class = "nr-preset"),
              lapply(seq_len(nrow(periods)), function(i) {
                shiny::actionLink(
                  session$ns(paste0("preset_", i)), periods$label[[i]],
                  class = "nr-preset"
                )
              })
            )
          }
        )
      })
    })

    shiny::observeEvent(input$preset_all, {
      shiny::req(bounds)
      shiny::updateSliderInput(session, "dates", value = bounds)
    })

    # One observer per period rather than a shared input, because
    # actionLink() has no value to carry -- local() so each closure keeps
    # its own i, which a bare for loop would not: every observer would
    # see the last one.
    for (i in seq_len(nrow(periods))) {
      local({
        idx <- i
        shiny::observeEvent(input[[paste0("preset_", idx)]], {
          shiny::req(bounds)
          shiny::updateSliderInput(
            session, "dates",
            value = preset_range(periods[idx, , drop = FALSE], bounds)
          )
        })
      })
    }

    shiny::reactive({
      if (is.null(bounds)) return(NULL)
      # The slider does not exist until renderUI has run, and the first
      # pass of every downstream reactive happens before it does. Falling
      # back to the full bounds means the view draws all the data on that
      # first pass rather than drawing nothing and flickering.
      if (is.null(input$dates)) bounds else as.Date(input$dates)
    })
  })
}

#' A named period, clipped to the data that exists
#'
#' CLIPPED, because a rotation is not a statement about this dataset. The
#' first rotation starts in 2017 and the earliest tournament abr records
#' is older than that; the newest period runs to today while the data
#' stops at the last sync. Handing a slider a value outside its own
#' min/max gives a control displaying a range that contains nothing, and
#' the reader has no way to tell that from a quiet period.
#'
#' @param period A one-row data frame from rotation_periods().
#' @param bounds Length-2 Date vector, the span of the data.
#' @return A length-2 Date vector inside `bounds`.
#' @keywords internal
preset_range <- function(period, bounds) {
  c(max(period$start[[1]], bounds[[1]]), min(period$end[[1]], bounds[[2]]))
}

#' Keep the tournaments inside a date range
#'
#' @param dated Output of with_parsed_dates().
#' @param range Length-2 Date vector, or NULL for everything.
#' @return The rows in range, or NULL when there is nothing to filter.
#' @keywords internal
filter_by_date <- function(dated, range) {
  if (is.null(dated)) return(NULL)
  if (is.null(range)) return(dated)
  keep <- !is.na(dated$.date) & dated$.date >= range[[1]] & dated$.date <= range[[2]]
  dated[keep, , drop = FALSE]
}
