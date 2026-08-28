# inst/shiny-app/

Shiny app served by `run_app()` from the installed package directory.

## Files

| File | What | When to read |
| --- | --- | --- |
| `app.R` | Calls `netrunneR::load_ice_breaker_app_data()` ONCE per process, then `shinyApp(ui = app_ui(), server = function(input, output, session) app_server(input, output, session, app_data))` -- the entry point `shiny::shinyAppDir()` actually looks for. Was missing entirely until the ice/breaker matchup app; `app_ui.R`/`app_server.R` alone are not a convention `shinyAppDir()` recognizes. Calls package functions with explicit `netrunneR::` prefixes rather than `library(netrunneR)` (this directory is sourced outside the package namespace, so a bare name would not resolve; explicit prefixes avoid attaching the whole package to the search path from inside a packaged app's own entry point) | Changing how the three files here are wired together, or what's loaded once vs. per session |
| `app_ui.R` | `app_ui()` -- a single server-rendered `uiOutput("main")`; renders no real content itself, since it runs once at app-definition time before any session (and therefore any release) exists | Implementing UI layout or adding a view |
| `app_server.R` | `app_server(input, output, session, app_data)` -- takes the already-loaded `app_data` from `app.R` (not resolved/recomputed per session), renders `startup_error_ui()` if `app_data$missing_lineages` is set, otherwise wires `mod_card_browser`/`mod_matchup_explorer`/`mod_card_detail` together through one shared `selected_code` reactiveVal | Implementing reactive server logic or wiring a view |

There is no separate "matchup" release to resolve: `compute_ice_breaker_matchups()` is a plain function called live against the active cardpool/implementation data, exactly like `compute_identity_ratings()`; `matchup` is not one of the five `BUILTIN_LINEAGES` (see `R/lineage.R`). `netrunneR::load_ice_breaker_app_data()` (`R/operations.R`) is what actually resolves the two releases, reads both SQLite databases, and computes the matchup table -- once per process, shared by every session, since none of this changes for the life of the R process.

Any view sourced from the `abr` lineage must render the `alwaysberunning.net` backlink and call `netrunneR::require_abr_attribution(TRUE)`; see `R/app.R`.

Any view sourced from the `implementation` lineage must render the mtgred/netrunner MIT copyright and permission notice and call `netrunneR::require_implementation_license_notice(netrunneR::IMPLEMENTATION_MIT_NOTICE_CONFIRMED)`; see `R/app.R`.

Any view sourced from the `cardpool` lineage must render a disclaimer that it is not maintained, produced, endorsed, supported, or affiliated with Fantasy Flight Games and/or Wizards of the Coast, and call `netrunneR::require_cardpool_disclaimer(netrunneR::CARDPOOL_DISCLAIMER_CONFIRMED)`; see `R/app.R`.

Any view sourced from the `rules` lineage must render a disclaimer that it is not associated with, produced by, or endorsed by Fantasy Flight Games, R. Talsorian Games, or Wizards of the Coast, and call `netrunneR::require_rules_disclaimer(<a gate constant>)`; see `R/app.R`.

## Run

```sh
Rscript -e 'netrunneR::run_app()'
```

Never pass a literal `TRUE` to any of these guards. They are `stopifnot()` assertions, so a literal makes them unfalsifiable and the guard decorative. Pass the pre-ship gate constant from `R/config.R`; if a lineage has no constant yet (rules), add one defaulting to `FALSE` rather than hardcoding the argument.
