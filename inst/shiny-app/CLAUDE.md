# inst/shiny-app/

Shiny app served by `run_app()` from the installed package directory.

## Files

| File | What | When to read |
| --- | --- | --- |
| `app.R` | Calls `netrunneR::load_ice_breaker_app_data()` ONCE per process, then `shinyApp(ui = app_ui(), server = function(input, output, session) app_server(input, output, session, app_data))` -- the entry point `shiny::shinyAppDir()` actually looks for. Was missing entirely until the ice/breaker matchup app; `app_ui.R`/`app_server.R` alone are not a convention `shinyAppDir()` recognizes. Calls package functions with explicit `netrunneR::` prefixes rather than `library(netrunneR)` (this directory is sourced outside the package namespace, so a bare name would not resolve; explicit prefixes avoid attaching the whole package to the search path from inside a packaged app's own entry point) | Changing how the three files here are wired together, or what's loaded once vs. per session |
| `app_ui.R` | `app_ui()` -- a single server-rendered `uiOutput("main")`; renders no real content itself, since it runs once at app-definition time before any session (and therefore any release) exists | Implementing UI layout or adding a view |
| `app_server.R` | `app_server(input, output, session, app_data)` -- takes the already-loaded `app_data` from `app.R` (not resolved/recomputed per session), renders `startup_error_ui()` if `app_data$missing_lineages` is set, otherwise mounts `mod_lane_board` as the WHOLE app and wires two `mod_card_browser` instances (ice pool, breaker pool) as add-card modals behind its ADD ICE / + slots, all sharing one `selected_code` reactiveVal with `mod_card_detail` | Implementing reactive server logic or adding a view |

## Layout

The lane board is the app. There is no navbar and no standalone Browse
tab: `Main.dc.html` in
`homelab/docs/netrunneR/design-references/wireframes/` is the landing
screen, and that canvas's own annotation on `SearchModal.dc.html` says
the add-card modal "replaces the standalone browse screen". So
`mod_card_browser` still does all the searching, filtering and legality
annotation -- only where it is rendered moved, from a peer tab into a
modal. Nothing in `mod_card_browser.R` changed to allow that: its
`selected_code` argument is only ever *called* with a code, so passing a
different `reactiveVal` is the whole of what redirects a click from
"open the detail modal" to "add this card to the board".

`mod_matchup_explorer` is reached from the card detail modal, not from a
view of its own. It was a "Matchup" tab under the old navbar and spent a
while exported, tested and mounted nowhere; the design corpus has no
artboard for it, so rather than re-introducing navigation the wireframe
deliberately does not have, the comparison hangs off a card you have
already opened.

Arriving with a card decides everything the old UI asked for: an ice
compares against breakers, a breaker against ice, so there is no mode
selector and no card pickers. The old "All vs all" mode is gone rather
than hidden -- it answers no question anyone actually has, and the two
single-card views are the ones the lane board cannot already do cheaply.

The two modals are mutually exclusive by construction. Both live in the
single `#shiny-modal` element and each dismisses itself before setting
the other's `reactiveVal`, so they hand off rather than stack. Each
carries a hidden marker div (`.dc-detail-modal` / `.dc-matchup-modal`)
because their `hidden.bs.modal` handlers are delegated on `document` and
would otherwise fire on each other's dismissal, clearing a value that had
just been set.

The matchup table itself is format-blind -- `compute_ice_breaker_matchups()`
takes no format argument, and the lane board reads the same unfiltered
table -- so the explorer filters at DISPLAY, defaulting to Standard, the
same way `mod_card_browser` does. Filtering at compute time would bake a
format into the view manifest's cache identity. Note the lane board has
no format control at all yet, which is the same gap in a live view.

## What a stat strip can say

The strip under a breaker has five states, and they are genuinely
different claims:

| badge | meaning |
| --- | --- |
| `FORMULA` | derived by `compute_cost_to_break_formula()` |
| `OVERRIDE` | hand-curated in `inst/extdata/matchup_overrides.csv` |
| `ASSUMED` | the same arithmetic, on a premise the operator supplied |
| `CANNOT BREAK` | the subtype filter excluded this pair on purpose |
| `NOT COMPUTABLE` | we do not know |

`CANNOT BREAK` and `NOT COMPUTABLE` were one state until the board could
tell them apart. Both appear as an absent matchup row, because
`compute_ice_breaker_matchups()` only emits subtype-compatible pairs and
the board lets the operator stack whatever they like. `traits` is what
separates them: a breaker that declares what it breaks and does not break
this is a definite no; a breaker whose break clause could not be read is
a genuine unknown, and reporting our parser's gap as a fact about the
card is the failure this codebase is built to avoid.

`ASSUMED` exists because subtypes change during a game. The override is
per lane -- keyed `"<ice_code>|<breaker_code>"`, the same composite key
`remove_breaker` uses, since the same breaker can sit under several ice
-- and it lives in a session `reactiveVal`, never in
`matchup_overrides.csv`. That file is for corrections true of the cards;
an override here is a claim about one board in front of one person.

There is no separate "matchup" release to resolve: `compute_ice_breaker_matchups()` is a plain function called live against the active cardpool/implementation data, exactly like `compute_identity_ratings()`; `matchup` is not one of the five `BUILTIN_LINEAGES` (see `R/lineage.R`). `netrunneR::load_ice_breaker_app_data()` (`R/operations.R`) is what actually resolves the two releases, reads both SQLite databases, and computes the matchup table -- once per process, shared by every session, since none of this changes for the life of the R process.

Any view sourced from the `abr` lineage must render the `alwaysberunning.net` backlink and call `netrunneR::require_abr_attribution(TRUE)`; see `R/app.R`.

Any view sourced from the `implementation` lineage must render the mtgred/netrunner MIT copyright and permission notice and call `netrunneR::require_implementation_license_notice(netrunneR::IMPLEMENTATION_MIT_NOTICE_CONFIRMED)`; see `R/app.R`.

Any view sourced from the `cardpool` lineage must render a disclaimer that it is not maintained, produced, endorsed, supported, or affiliated with Fantasy Flight Games and/or Wizards of the Coast, and call `netrunneR::require_cardpool_disclaimer(netrunneR::CARDPOOL_DISCLAIMER_CONFIRMED)`; see `R/app.R`.

Any view sourced from the `rules` lineage must render a disclaimer that it is not associated with, produced by, or endorsed by Fantasy Flight Games, R. Talsorian Games, or Wizards of the Coast, and call `netrunneR::require_rules_disclaimer(netrunneR::RULES_DISCLAIMER_CONFIRMED)`; see `R/app.R`.

## Run

```sh
Rscript -e 'netrunneR::run_app()'
```

Never pass a literal `TRUE` to any of these guards. They are `stopifnot()` assertions, so a literal makes them unfalsifiable and the guard decorative. Pass the pre-ship gate constant from `R/config.R`. All three lineages that have a guard now have one; if you add a fourth, add its constant defaulting to `FALSE` rather than hardcoding the argument.
