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
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "netrunner.css")
    ),
    shiny::uiOutput("main")
  )
}
