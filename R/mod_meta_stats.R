#' The meta statistics view
#'
#' Summary statistics about the tournament meta, over the same date
#' filter the map carries -- literally the same module, not a second
#' slider that looks like it (see mod_filter_bar_server()).
#'
#' Four figures, all adapted from the analysis notebook and all driven by
#' the same filtered rows: a treemap drilling side -> faction ->
#' identity, a filled area of faction share per quarter, a bump chart of
#' faction rank per quarter, and a chord diagram of which Runner and Corp
#' factions get brought together.
#'
#' GGPLOT2, TREEMAP AND D3TREER ARE SUGGESTS, for the reason sf and tmap
#' are (see mod_meta_map_ui()): in Imports the whole package fails to
#' load anywhere they are absent, which couples the repository to an
#' image rebuild for a view most sessions never open. Every entry point
#' here checks and degrades.
#'
#' @param id Module id.
#' @export
mod_meta_stats_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
      class = "nr-map-page",
      shiny::div(
        class = "nr-map-head",
        shiny::h4("Meta statistics"),
        shiny::uiOutput(ns("summary"))
      ),
      shiny::div(
        class = "nr-stats-block",
        shiny::h5("Faction share of wins"),
        # SAID OUT LOUD, above the chart. The treemap's two top-level
        # branches are exactly equal and always will be, because every
        # tournament records one Runner winner and one Corp winner -- the
        # two decks of the same person. A reader who is not told that
        # reads "the sides are evenly matched" off a picture that
        # measured nothing of the kind.
        shiny::tags$p(
          class = "small text-muted",
          "Every tournament records one Runner and one Corp winner, so the two ",
          "halves are equal by construction. What varies is inside them -- ",
          "click a half for its factions, then a faction for its identities."
        ),
        shiny::uiOutput(ns("treemap_slot"))
      ),
      shiny::div(
        class = "nr-stats-block",
        shiny::h5("Faction share over time"),
        shiny::tags$p(
          class = "small text-muted",
          paste(
            "Quarterly share of each side's wins, filled to 100%. The raw",
            "number of events in a quarter is a fact about the calendar",
            "rather than about the meta, so it is not what this asks.",
            "Hover a band for its faction, quarter, wins and share."
          )
        ),
        shiny::uiOutput(ns("area_slot"))
      ),
      shiny::div(
        class = "nr-stats-block",
        shiny::h5("Faction rank over time"),
        shiny::tags$p(
          class = "small text-muted",
          paste(
            "Rank 1 is the most-winning faction that quarter. A line breaks",
            "where a faction won nothing -- it has no rank, rather than a",
            "position among the other factions on zero. Hover a point for",
            "its faction, quarter, rank and wins."
          )
        ),
        shiny::uiOutput(ns("bump_slot"))
      ),
      shiny::div(
        class = "nr-stats-block",
        shiny::h5("Which factions are brought together"),
        shiny::tags$p(
          class = "small text-muted",
          paste(
            "One tournament, one link: the faction of its winning Runner",
            "against the faction of its winning Corp. Those are two decks the",
            "same person brought, so this says which pairs people win with,",
            "and nothing about which faction beats which."
          )
        ),
        shiny::uiOutput(ns("chord_slot"))
      ),
      abr_attribution_ui(),
      shiny::uiOutput(ns("notes"))
    )
  )
}

#' Meta statistics module server
#'
#' @param id Module id.
#' @param tournaments The abr `tournament` table, or NULL when no abr
#'   release is active. NULL renders an explanation rather than an empty
#'   chart, for the reason given on mod_card_detail_server()'s `rulings`.
#' @param identities The cardpool identity cards, NOT the app's
#'   ice/breaker pool. Winners are identities, and an identity is in
#'   neither of those two types -- handed the restricted pool this view
#'   would join nothing and draw an empty chart, which looks like a quiet
#'   meta rather than a wrong argument. The distinction is made in
#'   load_ice_breaker_app_data() so it cannot be got wrong here.
#' @param factions The cardpool `faction` table, or NULL.
#' @param filters A reactive returning the app-level filter selection,
#'   from mod_filter_bar_server(). The same reactive the map reads, so
#'   the two views cannot show different periods.
#' @export
mod_meta_stats_server <- function(id, tournaments = NULL, identities = NULL,
                                  factions = NULL, filters = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # The same gate the map passes, for the same reason: this view reads
    # abr data, and abr's terms attach to using the data rather than to
    # any particular drawing of it.
    require_abr_attribution(ABR_ATTRIBUTION_CONFIRMED)

    have_treemap <- requireNamespace("d3treeR", quietly = TRUE) &&
      requireNamespace("treemap", quietly = TRUE)
    have_ggplot <- requireNamespace("ggplot2", quietly = TRUE) &&
      requireNamespace("plotly", quietly = TRUE)
    have_chord <- requireNamespace("chorddiag", quietly = TRUE)

    dated <- with_parsed_dates(tournaments)

    draft_codes <- draft_identity_codes(identities)
    in_range <- shiny::reactive({
      apply_tournament_filters(
        dated, if (is.null(filters)) NULL else filters(), draft_codes
      )
    })

    # Everything below reads THIS, so the two charts cannot disagree
    # about which tournaments they drew.
    shaped <- shiny::reactive({
      d <- in_range()
      if (is.null(d) || !nrow(d)) return(NULL)
      tournament_faction_wins(d, identities, factions)
    })

    # EVERY FIGURE DERIVES FROM in_range(), so the one filter bar governs
    # all four and they cannot disagree about which tournaments they drew.
    identity_wins <- shiny::reactive({
      d <- in_range()
      if (is.null(d) || !nrow(d)) return(NULL)
      w <- tournament_identity_wins(d, identities, factions)
      if (!nrow(w)) NULL else w
    })

    quarterly <- shiny::reactive({
      d <- in_range()
      if (is.null(d) || !nrow(d)) return(NULL)
      q <- faction_quarterly_shares(d, identities, factions)
      if (!nrow(q)) NULL else q
    })

    pairings <- shiny::reactive({
      d <- in_range()
      if (is.null(d) || !nrow(d)) return(NULL)
      faction_pairing_matrix(d, identities, factions)
    })

    output$summary <- shiny::renderUI({
      safe_render(function() {
        s <- shaped()
        if (is.null(s) || !nrow(s$wins)) return(NULL)
        decided <- sum(s$wins$wins[s$wins$side == "Runner"])
        shiny::tags$p(class = "small text-muted", sprintf(
          "%s tournaments with a recorded winner%s.",
          format(decided, big.mark = ","),
          if (s$undecided) sprintf(
            "; %s more concluded without one and are not charted",
            format(s$undecided, big.mark = ",")
          ) else ""
        ))
      })
    })

    output$treemap_slot <- shiny::renderUI({
      safe_render(function() {
        if (!have_treemap) {
          return(alert_box(paste(
            "The treemap needs the d3treeR and treemap packages, which are not",
            "installed in this environment. The charts below read the same data."
          ), "info"))
        }
        if (is.null(identity_wins())) return(no_release_box())
        # WRAPPED IN A MIN-WIDTH SCROLLER. d3treeR sizes its boxes from
        # the container ONCE, at render, and ships no resize handler --
        # so a tab laid out at zero width (a background tab) draws a
        # treemap of zero-width rectangles and never redraws it. The
        # chord had the same failure and was fixed the same way.
        #
        # A min-width rather than a fixed width, so the map still fills a
        # wide screen; the scroller is what keeps a narrow one usable
        # instead of squashed.
        shiny::div(
          class = "nr-wide-scroll",
          d3treeR::d3tree2Output(session$ns("treemap"), height = "620px")
        )
      })
    })

    if (have_treemap) {
      # safe_render() is NOT used on a widget renderer: it returns an
      # alert_box() tag on error and a widget output cannot display a
      # shiny.tag. The same trap the map documents on its own renderer.
      # Messages belong in the slot above, which is a uiOutput and has
      # somewhere to put them.
      output$treemap <- d3treeR::renderD3tree2({
        w <- identity_wins()
        shiny::req(!is.null(w))
        h <- faction_treemap_hierarchy(w)
        shiny::req(!is.null(h))
        # LABELS ARE SIZED TO THEIR BOX AND HIDDEN WHEN THEY CANNOT FIT.
        # A treemap cannot repel labels the way a scatter plot can: every
        # rectangle is where its value puts it, so a label that does not
        # fit has nowhere to go. d3tree2 draws them all at one size
        # anyway, which turned the small identity boxes into overlapping
        # smears of text along the bottom of each faction.
        #
        # Drawing nothing there is the honest answer. The box is still
        # visible, still hoverable and still clickable, and a reader who
        # wants the small ones drills into the faction, where they get the
        # whole width to themselves.
        #
        # The timeout is because d3tree2 animates its zoom and the boxes
        # have no final geometry until that lands.
        htmlwidgets::onRender(
          d3treeR::d3tree2(h, width = "100%", height = "600px"),
          "
          function(el) {
            function fitLabels() {
              d3.select(el).selectAll('g.depth > g').each(function() {
                var g = d3.select(this);
                var box = g.select('rect.parent');
                var label = g.select('text');
                if (box.empty() || label.empty()) return;
                var w = +box.attr('width'), h = +box.attr('height');
                var chars = Math.max(label.text().length, 1);
                var size = Math.max(9, Math.min(22, w / chars * 1.7, h * 0.4));
                label
                  .attr('x', +box.attr('x') + w / 2)
                  .attr('y', +box.attr('y') + h / 2)
                  .attr('dy', '0.35em')
                  .attr('text-anchor', 'middle')
                  .style('font-size', size + 'px')
                  .style('font-weight', '600')
                  .style('pointer-events', 'none')
                  .style('display', (w < chars * 4.5 || h < 16) ? 'none' : null);
              });
            }
            fitLabels();
            el.addEventListener('click', function() {
              window.setTimeout(fitLabels, 800);
            });
          }
          "
        )
      })
    }

    # ONE HELPER for two of the three slots, because slots making the
    # same two decisions in slightly different ways is how one of them
    # ends up saying something the other does not.
    chart_slot <- function(ready, id, height) {
      if (!have_ggplot) {
        return(alert_box(paste(
          "This chart needs the ggplot2 and plotly packages, which are not",
          "installed in this environment."
        ), "info"))
      }
      if (!ready) return(no_release_box())
      plotly::plotlyOutput(session$ns(id), height = height)
    }

    output$area_slot <- shiny::renderUI({
      safe_render(function() chart_slot(!is.null(quarterly()), "area", "460px"))
    })

    output$bump_slot <- shiny::renderUI({
      safe_render(function() chart_slot(!is.null(quarterly()), "bump", "520px"))
    })

    if (have_ggplot) {
      # safe_render() is NOT used on a widget renderer: it returns an
      # alert_box() tag on error and a widget output cannot display a
      # shiny.tag. Messages belong in the slot above, which is a uiOutput.
      output$area <- plotly::renderPlotly({
        w <- nr_plotly(build_faction_area(quarterly()))
        shiny::req(!is.null(w))
        w
      })

      output$bump <- plotly::renderPlotly({
        w <- nr_plotly(build_faction_bump(quarterly()))
        shiny::req(!is.null(w))
        w
      })
    }

    # THE CHORD LIVES IN AN IFRAME, and this is the whole reason the
    # block below is more complicated than the three above it.
    #
    # chorddiag ships d3 4.13.0; d3treeR needs d3 v3 (`d3.scale`,
    # `d3.ease`, both removed in v4). htmlwidgets deduplicates
    # dependencies BY NAME and keeps the highest version, so putting both
    # widgets on one page loads exactly one d3 -- and the loser fails
    # silently. The treemap rendered as an empty div with no error of its
    # own; the only clue was chorddiag's d3 in the page source and
    # `d3.scale` coming back undefined.
    #
    # An iframe is a separate document with a separate global scope, so
    # each widget gets the d3 it was built against. The cost is that the
    # chord is serialised to self-contained HTML on every filter change,
    # and that the iframe inherits none of the page's CSS -- hence the
    # explicit background below rather than a class.
    output$chord_slot <- shiny::renderUI({
      safe_render(function() {
        if (!have_chord) {
          return(alert_box(paste(
            "The pairing diagram needs the chorddiag package, which is not",
            "installed in this environment. The charts above read the same data."
          ), "info"))
        }
        p <- pairings()
        if (is.null(p)) return(no_release_box())

        colours <- c(unname(FACTION_COLOURS[p$corp]),
                     unname(FACTION_COLOURS[p$runner]))
        colours[is.na(colours)] <- "#999999"

        widget <- chorddiag::chorddiag(
          p$matrix, type = "bipartite", groupColors = colours,
          categoryNames = c("Corp", "Runner"),
          groupnamePadding = 40, groupnameFontsize = 11,
          categorynameFontsize = 13, fadeLevel = 0.25,
          tooltipUnit = " tournaments", showTicks = FALSE,
          margin = 90, width = 720, height = 600
        )

        shiny::tags$iframe(
          class = "nr-chord-frame",
          # srcdoc rather than a served file: the widget is regenerated
          # per filter change, and a temp file per render would need its
          # own lifetime and cleanup for no benefit.
          srcdoc = chord_frame_html(widget),
          width = "100%", height = "640",
          frameborder = "0", scrolling = "no",
          # No allow-scripts, no chord. Nothing else is granted: the
          # document is generated here from our own data, but it is still
          # a document, and a sandbox that only permits what it needs is
          # the cheaper habit.
          sandbox = "allow-scripts"
        )
      })
    })

    output$notes <- shiny::renderUI({
      safe_render(function() {
        s <- shaped()
        if (is.null(s)) return(NULL)
        # A faction the colour table has never heard of is drawn grey and
        # named here rather than passed off as a design choice.
        unknown <- setdiff(s$wins$faction_code, FACTION_ORDER)
        if (!length(unknown)) return(NULL)
        alert_box(sprintf(
          "Drawn in grey, because there is no faction colour for: %s.",
          paste(sort(unknown), collapse = ", ")
        ), "warning")
      })
    })
  })
}

#' The no-abr-release message, shared by this view's chart slots
#' @keywords internal
no_release_box <- function() {
  alert_box(
    "No tournaments in range, so there is nothing to chart.", "info"
  )
}

#' Serialise a widget into a standalone HTML document
#'
#' The iframe needs a whole document, not a tag: `srcdoc` is parsed as
#' one, so a bare widget would arrive with no scripts at all.
#'
#' DEPENDENCIES ARE INLINED BY HAND rather than by
#' `saveWidget(selfcontained = TRUE)`, which requires **pandoc** -- and
#' pandoc is not in the sync image, so that call failed at runtime with
#' nothing but "Something went wrong displaying this" on the page. Adding
#' pandoc to the image would be tens of megabytes to concatenate a few
#' files this function can read directly.
#'
#' Inlining is also the point rather than a side effect: the frame has to
#' carry its OWN copy of d3, because sharing the parent's is the exact
#' collision the frame exists to avoid (see the chord slot above).
#'
#' The background is set inline because an iframe inherits no CSS from
#' the page around it, so the app's stylesheet cannot reach in.
#'
#' @param widget An htmlwidget.
#' @return A single string of HTML.
#' @keywords internal
chord_frame_html <- function(widget) {
  rendered <- htmltools::renderTags(widget)
  deps <- htmltools::resolveDependencies(rendered$dependencies)

  read_asset <- function(dep, file) {
    root <- if (!is.null(dep$package)) {
      system.file(dep$src$file, package = dep$package)
    } else {
      dep$src$file
    }
    path <- file.path(root, file)
    if (!file.exists(path)) return("")
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  }

  head_content <- vapply(deps, function(dep) {
    css <- vapply(dep$stylesheet %||% character(0), function(f) {
      sprintf("<style>%s</style>", read_asset(dep, f))
    }, character(1))
    js <- vapply(dep$script %||% character(0), function(f) {
      sprintf("<script>%s</script>", read_asset(dep, f))
    }, character(1))
    paste(c(css, js), collapse = "\n")
  }, character(1))

  sprintf(
    paste0(
      "<!DOCTYPE html><html><head><meta charset=\"utf-8\"/>%s</head>",
      "<body style=\"margin:0;background:%s;color:%s;",
      "font-family:system-ui,sans-serif\">%s</body></html>"
    ),
    paste(head_content, collapse = "\n"),
    unname(NETRUNNER_PALETTE[["panel_solid"]]),
    unname(NETRUNNER_PALETTE[["ink"]]),
    rendered$html
  )
}
