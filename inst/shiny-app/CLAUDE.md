# inst/shiny-app/

Shiny app served by `run_app()` from the installed package directory. Both entry points are stubs.

## Files

| File | What | When to read |
| --- | --- | --- |
| `app_ui.R` | `app_ui()` stub rendering a placeholder heading | Implementing UI layout or adding a view |
| `app_server.R` | `app_server()` stub with no server logic | Implementing reactive server logic or wiring a release into a view |

Any view sourced from the `abr` lineage must render the `alwaysberunning.net` backlink and call `netrunneR::require_abr_attribution(TRUE)`; see `R/app.R`.

## Run

```sh
Rscript -e 'netrunneR::run_app()'
```
