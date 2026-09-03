#' Shiny UI entry point
#'
#' Deliberately minimal and fully server-driven: app_ui() runs once at app
#' definition time (app.R's shinyApp(ui = app_ui(), ...) call), before any
#' session exists, so it cannot know yet whether cardpool/implementation
#' have active releases -- it only lays down a single server-rendered
#' slot. app_server() decides, per session, what fills it (the real tabs,
#' or the missing-release error screen).
app_ui <- function() {
  bslib::page_fluid(
    theme = netrunneR::netrunner_theme(),
    # Served from inst/shiny-app/www, which shinyAppDir() publishes at the
    # app root. Everything needing a selector lives there; everything
    # expressible as a Bootstrap variable lives in netrunner_theme().
    shiny::tags$head(
      # THE HREF CARRIES THE FILE'S MTIME. Without it the browser holds a
      # cached netrunner.css across app restarts -- so a stylesheet
      # change ships, the server serves it, and the page keeps rendering
      # against the old one. That is not merely slow to notice: it looks
      # exactly like a CSS change that did not work, and the obvious next
      # move is to "fix" a rule that was already correct.
      #
      # The value only has to change when the file does, so its mtime is
      # enough and needs no build step.
      shiny::tags$link(
        rel = "stylesheet", type = "text/css",
        href = sprintf("netrunner.css?v=%s", stylesheet_version())
      )
    ),
    # THE CHROME IS OUTSIDE `main`, and that is load-bearing rather than
    # tidy. Everything inside `main` is destroyed and rebuilt when the
    # reader changes view.
    #
    # The date filter cannot survive that. renderUI() caches: when the
    # uiOutput is recreated Shiny re-sends the HTML it built the FIRST
    # time, so the rebuilt slider came back at its original all-time
    # range and immediately reported that as the new selection --
    # silently undoing the reader's filter on every navigation, while the
    # summary line above it still claimed the filtered figure. Keeping
    # the widget mounted is the only fix that does not involve
    # out-thinking renderUI's cache.
    shiny::uiOutput("nav"),
    # A process-lifetime app has no other way to say which release it
    # is showing (R/operations.R's load_ice_breaker_app_data() loads
    # data once at startup) -- inline-styled rather than a new CSS
    # class, and kept out of suite_nav_ui() (R/mod_lane_board.R), a
    # shared, exported component with no other caller yet: nothing
    # here decides where a release caption belongs across a future
    # suite of apps, only that this one needs to show it somewhere.
    shiny::uiOutput("release_info"),
    # conditionalPanel HIDES rather than unmounts, which is the whole
    # point: the slider stays alive and keeps its value while the reader
    # is on a view that has no dates to filter. A server-side condition
    # would re-render and put us back where we started.
    shiny::conditionalPanel(
      condition = "input.nav_view == 'metaMaps' || input.nav_view == 'metaStats'",
      netrunneR::mod_filter_bar_ui("filters")
    ),
    shiny::uiOutput("main")
  )
}

#' A cache-busting token for the stylesheet
#'
#' The file's modification time, as an integer. Falls back to the session
#' start time when the file cannot be found -- a token that changes too
#' often costs one re-download, while one that never changes costs a
#' confusing afternoon.
#' @keywords internal
stylesheet_version <- function() {
  path <- system.file("shiny-app", "www", "netrunner.css", package = "netrunneR")
  if (!nzchar(path) || !file.exists(path)) return(as.integer(Sys.time()))
  as.integer(file.info(path)$mtime)
}
