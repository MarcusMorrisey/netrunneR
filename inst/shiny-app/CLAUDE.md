# inst/shiny-app/

Shiny app served by `run_app()` from the installed package directory.

## Files

| File | What | When to read |
| --- | --- | --- |
| `README.md` | Why the app has no navbar, what a stat strip's five states mean, and the attribution obligations each lineage carries | Before adding a view, a modal, or a lineage-sourced panel |
| `app.R` | Calls `netrunneR::load_ice_breaker_app_data()` ONCE per process, then `shinyApp(ui = app_ui(), server = function(input, output, session) app_server(input, output, session, app_data))` -- the entry point `shiny::shinyAppDir()` actually looks for. Was missing entirely until the ice/breaker matchup app; `app_ui.R`/`app_server.R` alone are not a convention `shinyAppDir()` recognizes. Calls package functions with explicit `netrunneR::` prefixes rather than `library(netrunneR)` (this directory is sourced outside the package namespace, so a bare name would not resolve; explicit prefixes avoid attaching the whole package to the search path from inside a packaged app's own entry point) | Changing how the three files here are wired together, or what's loaded once vs. per session |
| `app_ui.R` | `app_ui()` -- the page shell: a `uiOutput("nav")`, the filter bar inside a `conditionalPanel`, and the swapped `uiOutput("main")`. Renders no view content itself, since it runs once at app-definition time before any session exists | Implementing UI layout, adding a view, or changing what survives a view switch |
| `app_server.R` | `app_server(input, output, session, app_data)` -- takes the already-loaded `app_data` from `app.R` (not resolved/recomputed per session), renders `startup_error_ui()` if `app_data$missing_lineages` is set, otherwise instantiates every view module once, owns the `selected_code`/`compare_code` reactiveVals and the one filter bar, and routes `input$nav_view` to the view rendered into `main` | Implementing reactive server logic, adding a view, or changing what is shared between views |

## Subdirectories

| Directory | What | When to read |
| --- | --- | --- |
| `www/` | Static assets published at the app root | Changing app styling |

## Run

```sh
Rscript -e 'netrunneR::run_app()'
```
