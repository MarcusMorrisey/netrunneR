#' Add a parsed date column, once
#'
#' abr writes dates as "2026.08.27." and parse_abr_date() is not free.
#' Both meta views filter on date, and re-parsing 4,400 of those on every
#' slider drag is work with no answer attached to it.
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

#' Apply every filter the bar carries
#'
#' ONE FUNCTION, so a view cannot apply two of the three and quietly skip
#' the third. Each filter is a no-op when its input is NULL, which is how
#' a release that predates a column, or a caller with no filter bar at
#' all, gets served rather than errored at.
#'
#' @param dated Output of with_parsed_dates().
#' @param filters A list of `dates`, `types` and `include_draft`, from
#'   mod_filter_bar_server(). NULL applies nothing.
#' @param draft_codes Character vector from draft_identity_codes().
#' @return The surviving rows, or NULL when there is nothing to filter.
#' @keywords internal
apply_tournament_filters <- function(dated, filters, draft_codes = character(0)) {
  if (is.null(dated)) return(NULL)
  if (is.null(filters)) return(dated)

  keep <- rep(TRUE, nrow(dated))

  r <- filters$dates
  if (!is.null(r)) {
    keep <- keep & !is.na(dated$.date) & dated$.date >= r[[1]] & dated$.date <= r[[2]]
  }

  # An EMPTY type selection means every type, not no types. Unticking the
  # last chip should not blank the page -- a reader clearing a filter is
  # asking to see everything, and a chart that vanishes instead reads as
  # a crash.
  ty <- filters$types
  if (!is.null(ty) && length(ty) && "type" %in% names(dated)) {
    keep <- keep & !is.na(dated$type) & dated$type %in% ty
  }

  # Draft identities out by DEFAULT. See draft_identity_codes(): they are
  # the seven neutral-faction identities, they win 153 tournaments, and
  # 147 of those are drafts where both winners are draft identities. A
  # draft result says nothing about the constructed meta, which is what
  # these charts are for.
  #
  # Excluded, never deleted: the chip puts them back, because "which
  # events were drafts" is a real question and the count of what was
  # dropped is on the page either way.
  if (isFALSE(filters$include_draft) && length(draft_codes)) {
    keep <- keep &
      !(as.character(dated$winner_runner_identity) %in% draft_codes) &
      !(as.character(dated$winner_corp_identity) %in% draft_codes)
  }

  dated[keep, , drop = FALSE]
}

#' The filter bar, mounted once for the whole app
#'
#' A date slider with rotation shortcuts, chips for tournament type, and
#' a chip that puts draft identities back in.
#'
#' ONE INSTANCE, NOT ONE PER VIEW. Each view used to build its own date
#' filter. Setting the range on the stats page and switching to the map
#' left the map showing all 3,910 tournaments while the page you had just
#' left showed 723, through two controls that looked identical, sat in
#' the same place, and disagreed without saying so. app_server()
#' instantiates this once and hands the same reactive to both views.
#'
#' ITS UI IS MOUNTED OUTSIDE THE VIEW SLOT, in app_ui(), and that is
#' load-bearing rather than tidy: renderUI caches, so a filter rebuilt on
#' each navigation came back at its original range and silently undid the
#' reader's selection. See app_ui() for the whole of it.
#'
#' @param id Module id.
#' @export
mod_filter_bar_ui <- function(id) {
  shiny::uiOutput(shiny::NS(id, "filter"))
}

#' Filter bar module server
#'
#' @param id Module id.
#' @param tournaments The abr `tournament` table, or NULL. NULL renders
#'   no bar at all rather than controls over an invented range.
#' @param rotation The cardpool `rotation` table, or NULL. NULL drops the
#'   named-period shortcuts and leaves the slider, rather than inventing
#'   rotation dates -- there are seven and they are not guessable.
#' @param identities The cardpool identity cards, used only to work out
#'   which codes are draft identities. NULL drops the draft chip, because
#'   without the faction table there is no way to know which they are and
#'   a guess would silently drop real results.
#' @return A reactive returning a list of `dates` (length-2 Date),
#'   `types` (character, empty for all) and `include_draft` (logical).
#' @export
mod_filter_bar_server <- function(id, tournaments = NULL, rotation = NULL,
                                  identities = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    dated <- with_parsed_dates(tournaments)
    bounds <- date_bounds(dated)
    periods <- rotation_periods(
      rotation,
      max_date = if (is.null(bounds)) Sys.Date() else bounds[[2]]
    )
    types <- tournament_types(tournaments)
    draft_codes <- draft_identity_codes(identities)

    # THE SELECTION LIVES HERE, not in the widgets. They are destroyed
    # and rebuilt whenever the reader switches view; this is not, so the
    # filter survives the navigation that would otherwise reset it.
    state <- shiny::reactiveVal(list(
      dates = bounds, types = character(0), include_draft = FALSE
    ))

    set_state <- function(...) {
      cur <- shiny::isolate(state())
      state(utils::modifyList(cur, list(...)))
    }

    shiny::observeEvent(input$dates, set_state(dates = as.Date(input$dates)))

    # ignoreNULL = FALSE: clearing every chip sends NULL, and that is a
    # real selection meaning "all types". Without this the bar would
    # accept a reader ticking chips and ignore them unticking the last.
    shiny::observeEvent(input$types, ignoreNULL = FALSE, {
      set_state(types = if (is.null(input$types)) character(0) else input$types)
    })

    shiny::observeEvent(input$include_draft, ignoreNULL = FALSE, {
      set_state(include_draft = isTRUE(input$include_draft))
    })

    output$filter <- shiny::renderUI({
      safe_render(function() {
        if (is.null(bounds)) return(NULL)
        # isolate(): this renders once per view switch and must NOT
        # re-run when the selection changes, or every drag of the slider
        # would rebuild the slider underneath the reader's cursor.
        cur <- shiny::isolate(state())
        shiny::div(
          class = "nr-filter-bar",
          shiny::tags$details(
            # OPEN BY DEFAULT. A filter that starts collapsed is a filter
            # a first-time reader does not know is there.
            open = NA,
            class = "nr-filter-details",
            shiny::tags$summary(
              class = "nr-filter-summary",
              shiny::tags$span(class = "nr-filter-title", "Filters"),
              # THE APPLIED FILTERS ARE IN THE SUMMARY, so collapsing
              # hides the CONTROLS and never the answer. A collapsed
              # filter that does not say what it is filtering to turns
              # every figure under it into a number with an invisible
              # caveat.
              shiny::textOutput(session$ns("range_label"), inline = TRUE)
            ),
            shiny::div(
              class = "nr-filter-body",
              shiny::div(
                class = "nr-map-filter",
                shiny::sliderInput(
                  session$ns("dates"), NULL,
                  min = bounds[[1]], max = bounds[[2]],
                  value = cur$dates, timeFormat = "%b %Y", width = "100%"
                ),
                # A preset sets the STATE and moves the slider to match,
                # so it is a shortcut to a range and never a second,
                # competing filter.
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
              ),
              # THE CHIPS ARE ABSENT, NOT EMPTY, on a release built
              # before `type` was admitted to the abr allowlist. A row of
              # chips with nothing in it reads as "no tournaments match",
              # which is a claim about the data rather than about the
              # mirror. Saying so beats showing an empty control.
              if (nrow(types)) {
                shiny::div(
                  class = "nr-chip-row",
                  shiny::tags$span(class = "nr-preset-label", "Type:"),
                  shinyWidgets::checkboxGroupButtons(
                    session$ns("types"), label = NULL,
                    choices = stats::setNames(types$type, sprintf(
                      "%s (%s)", types$type, format(types$n, big.mark = ",")
                    )),
                    selected = cur$types,
                    status = "nr-chip", size = "sm", individual = TRUE
                  )
                )
              } else {
                # ONE STRING, not two children. htmltools puts each
                # child of a tag on its own line, so a sentence split
                # across two arrives with a newline through the middle of
                # it -- harmless on screen, where HTML collapses it, and
                # quietly unmatchable for anything reading the markup.
                shiny::tags$p(
                  class = "small text-muted nr-chip-row",
                  paste(
                    "No tournament types: the active abr release predates the",
                    "type column. The chips appear after the next sync."
                  )
                )
              },
              if (length(draft_codes)) {
                shiny::div(
                  class = "nr-chip-row",
                  shinyWidgets::materialSwitch(
                    session$ns("include_draft"),
                    label = "Include draft identities",
                    value = isTRUE(cur$include_draft),
                    status = "warning", inline = TRUE, right = TRUE
                  ),
                  shiny::tags$span(
                    class = "small text-muted",
                    "Off by default: a draft result says nothing about the ",
                    "constructed meta."
                  )
                )
              }
            )
          )
        )
      })
    })

    output$range_label <- shiny::renderText({
      f <- state()
      if (is.null(f$dates)) return("")
      parts <- paste(format(f$dates[[1]], "%b %Y"), "—",
                     format(f$dates[[2]], "%b %Y"))
      if (length(f$types)) {
        parts <- paste0(parts, "  ·  ", length(f$types), " of ",
                        nrow(types), " types")
      }
      if (isTRUE(f$include_draft)) parts <- paste0(parts, "  ·  incl. draft")
      parts
    })

    shiny::observeEvent(input$preset_all, {
      shiny::req(bounds)
      # BOTH: the state is what every figure reads, the slider is what
      # the reader sees. Setting only one leaves them disagreeing.
      set_state(dates = bounds)
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
          set_state(dates = r)
          shiny::updateSliderInput(session, "dates", value = r)
        })
      })
    }

    # The state IS the answer. It starts at the full bounds with drafts
    # excluded, so the first pass of every downstream reactive -- which
    # happens before renderUI has produced a control at all -- draws the
    # right thing rather than drawing nothing and flickering.
    shiny::reactive(state())
  })
}
