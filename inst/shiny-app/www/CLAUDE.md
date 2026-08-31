# inst/shiny-app/www/

Static assets, published at the app root by `shinyAppDir()` and linked from
`app_ui()`.

## Files

| File | What | When to read |
| --- | --- | --- |
| `netrunner.css` | The whole stylesheet: panels, the lane board, the suite nav, the sticky filter bar and its chips, and the leaflet controls the map draws | Changing anything that needs a CSS selector. Anything expressible as a Bootstrap variable belongs in `R/theme.R` instead |
