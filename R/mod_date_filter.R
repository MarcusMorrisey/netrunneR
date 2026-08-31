#' Add a parsed date column, once
#'
#' abr writes dates as "2026.08.27." and parse_abr_date() is not free.
#' Both meta views filter on date, and re-parsing 4,400 of those on every
#' slider drag is work with no answer attached to it -- so it happens
#' once, when the module is created, and everything downstream reads the
#' column.
#'
#' EXPORTED because app_server() calls it. The date filter is owned by
#' the app rather than by either view (see mod_date_filter_server()), so
#' the app is what needs the parsed dates to work out the slider's
#' bounds -- the same position rotation_periods() is already in.
#'
#' @param tournaments The abr `tournament` table, or NULL.
#' @return `tournaments` with a `.date` column, or NULL.
#' @export
with_parsed_dates <- function(tournaments) {
  if (is.null(tournaments) || !nrow(tournaments)) return(NULL)
  tournaments$.date <- parse_abr_date(tournaments$date)
  tournaments
}

#' The span of dates present in the data
#'
#' @param dated Output of with_parsed_dates().
#' @return A length-2 Date vector, or NULL when nothing parsed.
#' @export
date_bounds <- function(dated) {
  if (is.null(dated)) return(NULL)
  d <- dated$.date[!is.na(dated$.date)]
  if (!length(d)) NULL else range(d)
}

#' The date filter, mounted once for the whole app
#'
#' A slider whose two endpoints both move, plus named shortcuts for the
#' rotation periods.
#'
#' ONE INSTANCE, NOT ONE PER VIEW. It was written inline in the map,
#' then extracted so the stats view could mount its own copy -- and two
#' copies is exactly wrong. Setting the range on the stats page and
#' switching to the map left the map showing all 3,910 tournaments while
#' the page you had just left showed 723, with nothing on screen to say
#' the filter above the map was a different filter. Now app_server()
#' instantiates this once and hands the same reactive to both views, so
#' there is one date range in the app and every figure obeys it.
#'
#' IT REMEMBERS ITS RANGE ACROSS VIEW SWITCHES. Only one view is in the
#' DOM at a time, so switching destroys the slider and rebuilds it -- and
#' a slider rebuilt from `bounds` snaps back to all-time, silently
#' undoing the reader's filter on every navigation. The selection lives
#' in a reactiveVal that outlives the widget, and the slider is rendered
#' from THAT rather than from the bounds.
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
#'   range. NULL when there are no bounds at all.
#' @export
mod_date_filter_server <- function(id, bounds, periods) {
  shiny::moduleServer(id, function(input, output, session) {

    # THE SELECTION LIVES HERE, not in the slider. The slider is
    # destroyed and rebuilt every time the reader switches view; this is
    # not, so the range survives the navigation that would otherwise
    # reset it.
    state <- shiny::reactiveVal(bounds)

    shiny::observeEvent(input$dates, {
      state(as.Date(input$dates))
    })

    output$filter <- shiny::renderUI({
      safe_render(function() {
        if (is.null(bounds)) return(NULL)
        # isolate(): this renders once per view switch and must NOT
        # re-run when the range changes, or every drag of the slider
        # would rebuild the slider underneath the reader's cursor.
        current <- shiny::isolate(state())
        shiny::div(
          class = "nr-filter-bar",
          shiny::tags$details(
            # OPEN BY DEFAULT. A filter that starts collapsed is a filter
            # a first-time reader does not know is there.
            open = NA,
            class = "nr-filter-details",
            shiny::tags$summary(
              class = "nr-filter-summary",
              shiny::tags$span(class = "nr-filter-title", "Dates"),
              # THE RANGE IS IN THE SUMMARY, so collapsing the panel
              # hides the CONTROL and never the answer. A collapsed
              # filter that does not say what it is filtering to is a
              # chart with an invisible caveat.
              shiny::textOutput(session$ns("range_label"), inline = TRUE)
            ),
            shiny::div(
              class = "nr-map-filter nr-filter-body",
              shiny::sliderInput(
                session$ns("dates"), NULL,
                min = bounds[[1]], max = bounds[[2]],
                value = current, timeFormat = "%b %Y", width = "100%"
              ),
              # A preset sets the STATE and moves the slider to match, so
              # it is a shortcut to a range and never a second, competing
              # filter. The slider is not the source of truth any more --
              # the reactiveVal above is, because the slider stops
              # existing every time the reader changes view -- but the
              # two can never disagree about what is applied.
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
          )
        )
      })
    })

    output$range_label <- shiny::renderText({
      r <- state()
      if (is.null(r)) return("")
      paste(format(r[[1]], "%b %Y"), "\u2014", format(r[[2]], "%b %Y"))
    })

    shiny::observeEvent(input$preset_all, {
      shiny::req(bounds)
      # BOTH: the reactiveVal is what every figure reads, and the slider
      # is what the reader sees. Setting only the slider would leave the
      # charts on the old range until the browser echoed the change back;
      # setting only the state would leave the slider showing a range
      # that is not the one applied.
      state(bounds)
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
          r <- preset_range(periods[idx, , drop = FALSE], bounds)
          state(r)
          shiny::updateSliderInput(session, "dates", value = r)
        })
      })
    }

    # The state IS the answer. It starts at the full bounds, so the first
    # pass of every downstream reactive -- which happens before renderUI
    # has produced a slider at all -- draws all the data rather than
    # drawing nothing and flickering.
    shiny::reactive(state())
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
