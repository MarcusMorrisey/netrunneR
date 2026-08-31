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
#' @param filters A list of `dates` and `types`, from
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

  # TWO NARROWINGS, IN ORDER: the group, then the sub-types within it.
  #
  # The group has to be applied HERE and not merely used to populate the
  # picker. The first version of this did only the latter -- choosing
  # "Competitive" repopulated the dropdown and changed the collapsed
  # summary while every figure on the page carried on showing all 4,047
  # tournaments. A filter that announces itself and does nothing is worse
  # than no filter, because the label is evidence the reader trusts.
  if ("type" %in% names(dated)) {
    g <- filters$group
    if (!is.null(g) && !identical(g, "all")) {
      want <- TOURNAMENT_TYPE_GROUPS[[g]]
      if (!is.null(want)) keep <- keep & !is.na(dated$type) & dated$type %in% want
    }

    # An EMPTY sub-type selection means every type in the group, not no
    # types. Clearing the picker is a reader asking to widen back to the
    # group, and a chart that blanks instead reads as a crash.
    ty <- filters$types
    if (!is.null(ty) && length(ty)) {
      keep <- keep & !is.na(dated$type) & dated$type %in% ty
    }
  }

  # DRAFT IDENTITIES ARE ALWAYS OUT. Not a default with a switch beside
  # it -- there is no reading of these charts in which a draft result
  # belongs. See draft_identity_codes(): the seven neutral-faction
  # identities win 155 tournaments, 148 of them with a draft identity on
  # both sides, and a draft says nothing about the constructed meta.
  #
  # The switch that used to restore them is gone. A control offering to
  # put wrong data back into a chart is a control nobody should use, and
  # keeping it meant every figure carried "unless someone flipped this".
  if (length(draft_codes)) {
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
#'   `group` ("all", "competitive" or "casual") and `types`
#'   (character, empty meaning every type in that group).
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
    # `group` narrows to competitive or casual; `types` narrows further
    # WITHIN that group. Two controls rather than sixteen chips: sixteen
    # chips is a wall a reader has to finish reading before they can use
    # it, and the first question almost anyone has is "does this count
    # championship results or club nights".
    DEFAULTS <- list(dates = bounds, group = "all", types = character(0))
    state <- shiny::reactiveVal(DEFAULTS)

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

    # Changing group CLEARS the sub-type selection. Carrying it across
    # would leave a competitive sub-type selected under "Casual", which
    # matches nothing -- and an empty chart reads as an empty dataset
    # rather than as a contradiction the reader just built.
    shiny::observeEvent(input$group, {
      g <- if (is.null(input$group)) "all" else input$group
      set_state(group = g, types = character(0))
      shinyWidgets::updatePickerInput(
        session, "types",
        choices = types_in_group(g, types), selected = character(0)
      )
    })

    # CLEAR RESETS THE WIDGETS TOO, not just the state. The state is what
    # the figures read and the widgets are what the reader believes; a
    # clear that moved only one would leave the page disagreeing with its
    # own controls.
    shiny::observeEvent(input$clear, {
      state(DEFAULTS)
      if (!is.null(bounds)) {
        shiny::updateSliderInput(session, "dates", value = bounds)
      }
      shinyWidgets::updateRadioGroupButtons(session, "group", selected = "all")
      shinyWidgets::updatePickerInput(
        session, "types",
        choices = types_in_group("all", types), selected = character(0)
      )
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
              shiny::textOutput(session$ns("range_label"), inline = TRUE),
              # IN THE SUMMARY ROW, so it is reachable while the panel is
              # collapsed -- which is exactly when a reader has lost track
              # of what is applied and wants it gone.
              #
              # The inline handler stops the click reaching <summary>,
              # whose default action is to toggle the panel: without it,
              # clearing the filters also opens or closes them.
              shiny::actionLink(
                session$ns("clear"), "Clear all", class = "nr-clear-all",
                onclick = "event.preventDefault(); event.stopPropagation();"
              )
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
              # THE CONTROLS ARE ABSENT, NOT EMPTY, on a release built
              # before `type` was admitted to the abr allowlist. A control
              # with nothing in it reads as "no tournaments match", which
              # is a claim about the data rather than about the mirror.
              if (nrow(types)) {
                shiny::div(
                  class = "nr-chip-row",
                  shiny::tags$span(class = "nr-preset-label", "Events:"),
                  # EXCLUSIVE, so the group is a lens rather than a set.
                  # All, competitive and casual are not things you combine.
                  shinyWidgets::radioGroupButtons(
                    session$ns("group"), label = NULL,
                    choices = c("All" = "all", "Competitive" = "competitive",
                                "Casual" = "casual"),
                    selected = cur$group,
                    status = "nr-chip", size = "sm", individual = TRUE
                  ),
                  # The sub-types of whichever group is chosen. Selecting
                  # none means all of them, so the picker starts empty
                  # rather than starting with everything ticked.
                  shinyWidgets::pickerInput(
                    session$ns("types"), label = NULL,
                    choices = types_in_group(cur$group, types),
                    selected = cur$types, multiple = TRUE, width = "250px",
                    options = shinyWidgets::pickerOptions(
                      actionsBox = TRUE, liveSearch = TRUE,
                      selectedTextFormat = "count > 1",
                      countSelectedText = "{0} event types",
                      noneSelectedText = "All in this group"
                    )
                  ),
                  # NAMED, not absorbed. A type abr adds after
                  # TOURNAMENT_TYPE_GROUPS was written is a decision
                  # waiting to be made; it stays reachable under All and
                  # says so, rather than being defaulted into a group
                  # nobody chose for it.
                  if (length(tournament_types_ungrouped(types))) {
                    shiny::tags$span(
                      class = "small text-muted",
                      sprintf("Unclassified, so only under All: %s.",
                              paste(tournament_types_ungrouped(types),
                                    collapse = ", "))
                    )
                  }
                )
              } else {
                # ONE STRING, not two children. htmltools puts each child
                # of a tag on its own line, so a sentence split across two
                # arrives with a newline through the middle of it.
                shiny::tags$p(
                  class = "small text-muted nr-chip-row",
                  paste(
                    "No tournament types: the active abr release predates the",
                    "type column. The controls appear after the next sync."
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
      if (!identical(f$group, "all")) {
        parts <- paste0(parts, "  ·  ", f$group)
      }
      if (length(f$types)) {
        parts <- paste0(parts, "  ·  ", length(f$types), " of ",
                        length(types_in_group(f$group, types)), " types")
      }
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
