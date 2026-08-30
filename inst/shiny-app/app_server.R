#' Shiny server entry point
#'
#' Receives `app_data` (built once per process by
#' `netrunneR::load_ice_breaker_app_data()`, see `inst/shiny-app/app.R`)
#' rather than resolving releases and recomputing matchups itself --
#' cardpool/implementation data is static for the life of the R process,
#' so recomputing per Shiny session (opening two SQLite connections and
#' rerunning the ice x breaker cross-join on every browser tab) would be
#' pure waste.
#'
#' LAYOUT: the lane board is the whole app, per Main.dc.html in
#' homelab/docs/netrunneR/design-references/wireframes/. There is no
#' navbar and no standalone Browse tab: the wireframe's own annotation on
#' SearchModal.dc.html says the add-card modal "replaces the standalone
#' browse screen", so the browser module is mounted inside a modal rather
#' than as a peer view. `mod_card_browser_*` is unchanged and still does
#' the searching, filtering and legality annotation -- only where it is
#' rendered has moved.
#'
#' The matchup view is mounted the same way, as a modal reached from the
#' card detail modal rather than as a view of its own. It was a "Matchup"
#' tab under the old navbar and was left mounted nowhere when the lane
#' board replaced it; the design corpus has no artboard for it, so it is
#' reached from a card instead of being given a destination the wireframe
#' does not have.
#'
#' @param app_data The value `netrunneR::load_ice_breaker_app_data()` returns.
app_server <- function(input, output, session, app_data) {
  if (!is.null(app_data$missing_lineages)) {
    output$main <- shiny::renderUI(startup_error_ui(app_data$missing_lineages))
    return(invisible(NULL))
  }

  cards <- app_data$cards
  matchup <- app_data$matchup

  # Selected-card state for the detail modal is owned here, once, for the
  # whole session. Every view that can open a card receives the same
  # setter and only ever calls it; none instantiates its own copy of the
  # detail module.
  selected_code <- shiny::reactiveVal(NULL)

  # A SECOND caller-owned reactiveVal, exactly parallel to selected_code
  # and for the same reason: the matchup modal is one module instantiated
  # once per session, driven by a value this function owns, not a module
  # rebuilt per click.
  #
  # The two are deliberately never set at the same time. Each modal
  # replaces the other rather than stacking -- Bootstrap nests modals
  # poorly, and both share the single `#shiny-modal` element -- so the
  # handoff in each direction dismisses the modal it is leaving before
  # setting the other value. See the marker div in each module for how
  # their dismissal handlers are kept from firing on each other.
  compare_code <- shiny::reactiveVal(NULL)

  netrunneR::mod_card_detail_server(
    "card_detail_modal", selected_code, cards,
    app_data$rulings, app_data$legality,
    # Card detail is the ONLY way into the matchup view. There is no
    # navbar and the wireframe (Main.dc.html) does not draw one, so the
    # comparison hangs off the card you already opened rather than
    # becoming a peer destination. That also means the matchup module
    # needs no mode selector and no card pickers: you arrive with a card,
    # and its type decides which side of the pair you are asking about.
    on_compare = function(code) compare_code(code)
  )

  netrunneR::mod_matchup_explorer_server(
    "matchup_modal", compare_code, cards, matchup, selected_code,
    traits = app_data$traits, legality = app_data$legality
  )

  # The two picker modules are instantiated ONCE per session, not per
  # click. mod_card_browser_server() registers observers, so building a
  # fresh one each time the modal opened would accumulate a growing set
  # per session -- the same reasoning documented on
  # mod_card_detail_server().
  #
  # Each receives a pre-filtered pool rather than a "restrict to type"
  # argument: the browser's contract is "show me this data frame", and
  # narrowing the data frame is how a caller says which half of the pool
  # it means. That is also why nothing in mod_card_browser.R had to
  # change to support being used as a picker -- `selected_code` is only
  # ever called with a code, so passing a different reactiveVal is
  # enough to redirect a click from "open detail" to "add to board".
  ice_pool <- cards[cards$type_code == "ice", , drop = FALSE]
  breaker_pool <- cards[
    cards$type_code == "program" &
      cards$side_code == "runner" &
      !is.na(cards$keywords) &
      grepl("Icebreaker", cards$keywords, fixed = TRUE), ,
    drop = FALSE
  ]

  # Computed ONCE per session, from the pool each picker is mounted over,
  # and built into that picker's UI. See mod_card_browser_ui(): pushing
  # these from the server is what left every filter empty once this module
  # moved into a modal.
  ice_choices <- netrunneR::browser_choices(ice_pool, app_data$legality)
  breaker_choices <- netrunneR::browser_choices(breaker_pool, app_data$legality)

  picked_ice <- shiny::reactiveVal(NULL)
  picked_breaker <- shiny::reactiveVal(NULL)
  netrunneR::mod_card_browser_server("pick_ice", ice_pool, picked_ice, app_data$legality)
  netrunneR::mod_card_browser_server("pick_breaker", breaker_pool, picked_breaker, app_data$legality)

  board <- netrunneR::mod_lane_board_server(
    "board", cards, matchup, selected_code,
    # Only so the board can tell a pair the subtype filter dropped ON
    # PURPOSE from one it dropped because the breaker could not be read.
    traits = app_data$traits,
    on_add_ice = function() show_picker_modal(session, "pick_ice", "Add ice", ice_choices),
    on_add_breaker = function() show_picker_modal(session, "pick_breaker", "Add breaker", breaker_choices)
  )

  shiny::observeEvent(picked_ice(), {
    shiny::req(picked_ice())
    board$add_ice(picked_ice())
    picked_ice(NULL)
    shiny::removeModal()
  })

  shiny::observeEvent(picked_breaker(), {
    shiny::req(picked_breaker())
    board$add_breaker(picked_breaker())
    picked_breaker(NULL)
    shiny::removeModal()
  })

  # WHICH VIEW IS SHOWING. The nav strip sets a non-namespaced `nav_view`
  # input (see suite_nav_ui()); this is the only place that reads it,
  # because choosing a view is the app's job rather than any module's.
  #
  # Both view modules are instantiated ONCE, above and below, on the same
  # one-instantiation-per-session discipline as the card detail modal --
  # only which UI is rendered changes here. Switching views therefore
  # keeps the board's lanes and the map's state intact rather than
  # rebuilding them on every click.
  view <- shiny::reactiveVal("iceBreaker")
  shiny::observeEvent(input$nav_view, {
    if (input$nav_view %in% c("iceBreaker", "metaMaps")) view(input$nav_view)
  })

  netrunneR::mod_meta_map_server("meta_map", app_data$tournaments,
                                 rotation = app_data$legality$rotation)

  output$main <- shiny::renderUI({
    switch(view(),
      metaMaps = netrunneR::mod_meta_map_ui("meta_map"),
      netrunneR::mod_lane_board_ui("board")
    )
  })
}

#' Show one of the two card pickers as a modal
#'
#' The modal is built fresh on each open, but the module behind it is
#' not (see app_server()): this only renders the already-wired module's
#' UI into a dialog. `size = "xl"` because the browser's own layout is a
#' sidebar of filters plus an image grid, which is unreadable at the
#' default modal width.
#' @keywords internal
show_picker_modal <- function(session, module_id, title, choices) {
  shiny::showModal(shiny::modalDialog(
    title = title,
        # side = NULL: each picker is opened from a slot that can only
    # hold one kind of card and is mounted over a pool already
    # narrowed to it, so a Side control could only ever restate the
    # choice already made or empty the grid.
    netrunneR::mod_card_browser_ui(session$ns(module_id), side = NULL, choices = choices),
    size = "xl", easyClose = TRUE,
    footer = shiny::modalButton("Cancel")
  ))
}

#' Render the missing-release startup error screen
#' @keywords internal
startup_error_ui <- function(missing_lineages) {
  netrunneR::alert_box(sprintf(
    "No active release for: %s. Run a sync and promote before starting the app.",
    paste(missing_lineages, collapse = ", ")
  ), "danger")
}
