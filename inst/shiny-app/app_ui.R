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
    theme = bslib::bs_theme(),
    shiny::uiOutput("main")
  )
}
