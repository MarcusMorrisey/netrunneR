#' The meta statistics view
#'
#' Summary statistics about the tournament meta, over the same date
#' filter the map carries -- literally the same module, not a second
#' slider that looks like it (see mod_date_filter_server()).
#'
#' Two charts to start with, both adapted from the analysis notebook: an
#' interactive treemap of faction wins that drills into a side, and a
#' waffle of the same wins as percentage squares.
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
          "halves are equal by construction. What varies is the proportions ",
          "inside each -- click a half to drill into it."
        ),
        shiny::uiOutput(ns("treemap_slot"))
      ),
      shiny::div(
        class = "nr-stats-block",
        shiny::h5("One square, one percent"),
        shiny::tags$p(
          class = "small text-muted",
          "A hundred squares per side. Each is one percent of that side's wins."
        ),
        shiny::uiOutput(ns("waffle_slot")),
        shiny::uiOutput(ns("waffle_note"))
      ),
      abr_attribution_ui(),
      shiny::uiOutput(ns("notes")),
      shiny::uiOutput(ns("misfiled"))
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
#' @param selected A reactive returning the selected date range, from the
#'   app-level mod_date_filter_server(). The same reactive the map reads,
#'   so the two views cannot show different periods.
#' @export
mod_meta_stats_server <- function(id, tournaments = NULL, identities = NULL,
                                  factions = NULL, selected = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # The same gate the map passes, for the same reason: this view reads
    # abr data, and abr's terms attach to using the data rather than to
    # any particular drawing of it.
    require_abr_attribution(ABR_ATTRIBUTION_CONFIRMED)

    have_treemap <- requireNamespace("d3treeR", quietly = TRUE) &&
      requireNamespace("treemap", quietly = TRUE)
    have_ggplot <- requireNamespace("ggplot2", quietly = TRUE)

    dated <- with_parsed_dates(tournaments)

    in_range <- shiny::reactive({
      filter_by_date(dated, if (is.null(selected)) NULL else selected())
    })

    # Everything below reads THIS, so the two charts cannot disagree
    # about which tournaments they drew.
    shaped <- shiny::reactive({
      d <- in_range()
      if (is.null(d) || !nrow(d)) return(NULL)
      tournament_faction_wins(d, identities, factions)
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
            "installed in this environment. The waffle below reads the same data."
          ), "info"))
        }
        if (is.null(shaped())) return(no_release_box())
        d3treeR::d3tree2Output(session$ns("treemap"), height = "520px")
      })
    })

    if (have_treemap) {
      # safe_render() is NOT used on a widget renderer: it returns an
      # alert_box() tag on error and a widget output cannot display a
      # shiny.tag. The same trap the map documents on its own renderer.
      # Messages belong in the slot above, which is a uiOutput and has
      # somewhere to put them.
      output$treemap <- d3treeR::renderD3tree2({
        s <- shaped()
        shiny::req(!is.null(s))
        h <- faction_treemap_hierarchy(s$wins)
        shiny::req(!is.null(h))
        d3treeR::d3tree2(h, width = "100%", height = "500px")
      })
    }

    output$waffle_slot <- shiny::renderUI({
      safe_render(function() {
        if (!have_ggplot) {
          return(alert_box(paste(
            "The waffle needs the ggplot2 package, which is not installed in",
            "this environment."
          ), "info"))
        }
        if (is.null(shaped())) return(no_release_box())
        # 700px, NOT the 420 this started at. coord_equal() makes the
        # squares square by fixing the panel aspect, and two 20x5 panels
        # at full width want about 570px between them before the legend
        # and the two strip labels. Given less, ggplot does not shrink --
        # it aborts the whole plot with "figure margins too large", so
        # the chart is replaced by an error string rather than a smaller
        # waffle. Sized for the wide case; a narrow viewport just leaves
        # space underneath, which theme_void() renders as nothing.
        shiny::plotOutput(session$ns("waffle"), height = "700px")
      })
    })

    if (have_ggplot) {
      output$waffle <- shiny::renderPlot({
        s <- shaped()
        shiny::req(!is.null(s))
        p <- build_faction_waffle(s$wins)
        shiny::req(!is.null(p))
        p
      }, res = 96, bg = "transparent")
    }

    output$waffle_note <- shiny::renderUI({
      safe_render(function() {
        s <- shaped()
        if (is.null(s) || !nrow(s$wins)) return(NULL)
        small <- factions_below_resolution(s$wins)
        if (!nrow(small)) return(NULL)
        shiny::tags$p(class = "small text-muted", sprintf(
          "Too few wins to fill a square, so not drawn: %s.",
          paste(sprintf("%s (%s, %.1f%% of %s)", small$faction,
                        format(small$wins, big.mark = ","),
                        small$share * 100, tolower(small$side)),
                collapse = "; ")
        ))
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

    output$misfiled <- shiny::renderUI({
      safe_render(function() {
        s <- shaped()
        if (is.null(s) || !s$misfiled) return(NULL)
        # Named rather than quietly dropped: these are wins that exist and
        # are not in the chart, and the reason is an upstream entry error
        # rather than anything this view decided.
        alert_box(sprintf(paste(
          "%s %s credited to a faction from the other side and left out --",
          "a Corp identity recorded as the Runner winner, or the reverse.",
          "The upstream entry is wrong; there is no reading of the game in",
          "which they are right."
        ), s$misfiled, if (s$misfiled == 1) "win was" else "wins were"), "info")
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
