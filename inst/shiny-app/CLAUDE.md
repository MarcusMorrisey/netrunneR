# inst/shiny-app/

Shiny app served by `run_app()` from the installed package directory.

## Files

| File | What | When to read |
| --- | --- | --- |
| `app.R` | `shinyApp(ui = app_ui(), server = app_server)` -- the entry point `shiny::shinyAppDir()` actually looks for. Was missing entirely until the ice/breaker matchup app; `app_ui.R`/`app_server.R` alone are not a convention `shinyAppDir()` recognizes. Attaches the package with `library(netrunneR)` so the other two files can call exported package functions as bare names (this directory is sourced outside the package namespace). | Changing how the three files here are wired together |
| `app_ui.R` | `app_ui()` -- a single server-rendered `uiOutput("main")`; renders no real content itself, since it runs once at app-definition time before any session (and therefore any release) exists | Implementing UI layout or adding a view |
| `app_server.R` | `app_server()` -- resolves the active cardpool/implementation releases, renders `startup_error_ui()` if either is missing, otherwise loads both tables, computes matchups via `compute_ice_breaker_matchups()`, and wires `mod_card_browser`/`mod_matchup_explorer`/`mod_card_detail` together through one shared `selected_code` reactiveVal | Implementing reactive server logic or wiring a release into a view |

There is no separate "matchup" release to resolve: `compute_ice_breaker_matchups()` is a plain function called live against the active cardpool/implementation data, exactly like `compute_identity_ratings()`; `matchup` is not one of the five `BUILTIN_LINEAGES` (see `R/lineage.R`).

Any view sourced from the `abr` lineage must render the `alwaysberunning.net` backlink and call `netrunneR::require_abr_attribution(TRUE)`; see `R/app.R`.

Any view sourced from the `implementation` lineage must render the mtgred/netrunner MIT copyright and permission notice and call `netrunneR::require_implementation_license_notice(TRUE)`; see `R/app.R`.

Any view sourced from the `cardpool` lineage must render a disclaimer that it is not maintained, produced, endorsed, supported, or affiliated with Fantasy Flight Games and/or Wizards of the Coast, and call `netrunneR::require_cardpool_disclaimer(TRUE)`; see `R/app.R`.

Any view sourced from the `rules` lineage must render a disclaimer that it is not associated with, produced by, or endorsed by Fantasy Flight Games, R. Talsorian Games, or Wizards of the Coast, and call `netrunneR::require_rules_disclaimer(TRUE)`; see `R/app.R`.

## Run

```sh
Rscript -e 'netrunneR::run_app()'
```
