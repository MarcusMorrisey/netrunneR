# inst/shiny-app

## Overview

Three files wire an app whose behaviour lives almost entirely in the package's
`mod_*` modules. `app.R` is the entry point `shinyAppDir()` looks for; `app_ui()`
lays down a shell; `app_server()` owns every piece of state that more than one
view touches. Nothing here computes anything.

## Architecture

**One load per process, not per session.** `app.R` calls
`load_ice_breaker_app_data()` once and hands the result to every session.
Cardpool and implementation data is static for the life of the R process, so
resolving releases and recomputing the ice x breaker cross-join per browser tab
would be pure waste.

**There is no matchup release.** `compute_ice_breaker_matchups()` is a plain
function called live against the active cardpool and implementation data,
exactly like `compute_identity_ratings()`. `matchup` is not one of the five
`BUILTIN_LINEAGES`.

**Explicit `netrunneR::` prefixes, never `library(netrunneR)`.** This directory
is sourced outside the package namespace, so a bare name would not resolve, and
attaching the whole package to the search path from inside a packaged app's own
entry point is worse than qualifying four calls.

### What is chrome and what is a view

`app_ui()` lays down three slots: the nav strip, the filter bar, and `main`.
Only `main` is swapped when the reader changes view.

That split is load-bearing rather than tidy. `renderUI` caches, so a control
rebuilt on every navigation is re-sent as the HTML it was first given -- a date
filter mounted inside a view came back at its original range and silently undid
the reader's selection on each switch. The filter bar sits in a
`conditionalPanel`, which hides rather than unmounts, so its widgets stay alive
while the reader is on a view that has no dates to filter.

There is therefore exactly one filter in the app. Each meta view used to build
its own, and the two disagreed without saying so.

### The lane board is the app

There is no navbar in the wireframe corpus and no standalone Browse tab. The
landing screen's own annotation says the add-card modal "replaces the standalone
browse screen", so `mod_card_browser` still does all the searching, filtering
and legality annotation -- only where it renders moved, from a peer tab into a
modal.

Nothing in `mod_card_browser.R` had to change for that. Its `selected_code`
argument is only ever *called* with a code, so passing a different `reactiveVal`
is the whole of what redirects a click from "open the detail modal" to "add this
card to the board".

`mod_matchup_explorer` is reached from the card detail modal rather than from a
view of its own. It was a tab under the old navbar and spent a while exported,
tested and mounted nowhere. Arriving with a card decides everything the old UI
asked the reader for: an ice compares against breakers, a breaker against ice,
so there is no mode selector and no card pickers. The old "all versus all" mode
is gone rather than hidden -- it answers no question anyone has, and the two
single-card views are the ones the lane board cannot already do cheaply.

**The two modals are mutually exclusive by construction.** Both live in the
single `#shiny-modal` element and each dismisses itself before setting the
other's `reactiveVal`, so they hand off rather than stack. Each carries a hidden
marker div (`.dc-detail-modal`, `.dc-matchup-modal`) because their
`hidden.bs.modal` handlers are delegated on `document` and would otherwise fire
on each other's dismissal, clearing a value that had just been set.

**Format is filtered at display, not at compute.** The matchup table is
format-blind: `compute_ice_breaker_matchups()` takes no format argument and the
lane board reads the same unfiltered table. Filtering at compute time would bake
a format into the view manifest's cache identity. The lane board still has no
format control at all, which is the same gap in a live view.

**Deck Compare is a fourth `suite_nav_ui()` destination, not a modal.**
Every other view besides the lane board itself is either the lane board
or something reached from a card already on screen -- the matchup
explorer opens off the card detail modal, the two pickers open off the
"add ice"/"add breaker" slots. Deck Compare has no card to open from: it
is entered with nothing selected and supplies its own two deck-reference
inputs, so there is no opener to hang a modal from. It is mounted like
`mod_meta_map_ui()`/`mod_meta_stats_ui()` instead, swapped into `main` by
`nav_view`, and `mod_deck_compare_server()` is instantiated once beside
those two on the same one-instantiation-per-session discipline -- so
switching to another view and back keeps whatever deck was fetched
rather than refetching it.

## What a stat strip can say

The strip under a breaker has five states, and they are genuinely different
claims:

| badge | meaning |
| --- | --- |
| `FORMULA` | derived by `compute_cost_to_break_formula()` |
| `OVERRIDE` | hand-curated in `inst/extdata/matchup_overrides.csv` |
| `ASSUMED` | the same arithmetic, on a premise the operator supplied |
| `CANNOT BREAK` | the subtype filter excluded this pair on purpose |
| `NOT COMPUTABLE` | we do not know |

`CANNOT BREAK` and `NOT COMPUTABLE` were one state until the board could tell
them apart. Both appear as an absent matchup row, because
`compute_ice_breaker_matchups()` only emits subtype-compatible pairs while the
board lets the operator stack whatever they like. `traits` is what separates
them: a breaker that declares what it breaks and does not break this is a
definite no; a breaker whose break clause could not be read is a genuine
unknown. Reporting our parser's gap as a fact about the card is the failure this
codebase exists to avoid.

`ASSUMED` exists because subtypes change during a game. The override is per
lane -- keyed `"<ice_code>|<breaker_code>"`, the same composite key
`remove_breaker` uses, since one breaker can sit under several ice -- and it
lives in a session `reactiveVal`, never in `matchup_overrides.csv`. That file is
for corrections true of the cards; an override here is a claim about one board
in front of one person.

## Attribution obligations

Each mirrored lineage carries a notice that must render wherever its data does,
and a `require_*()` guard asserting it. All four live in `R/app.R`.

| Lineage | Must render | Guard |
| --- | --- | --- |
| `abr` | The `alwaysberunning.net` backlink | `require_abr_attribution()` |
| `implementation` | The mtgred/netrunner MIT copyright and permission notice | `require_implementation_license_notice()` |
| `cardpool` | Not maintained, produced, endorsed, supported or affiliated with Fantasy Flight Games and/or Wizards of the Coast | `require_cardpool_disclaimer()` |
| `rules` | Not associated with, produced by or endorsed by Fantasy Flight Games, R. Talsorian Games or Wizards of the Coast | `require_rules_disclaimer()` |

**Never pass a literal `TRUE` to any of these.** They are `stopifnot()`
assertions, so a literal makes them unfalsifiable and the guard decorative. Pass
the pre-ship constant from `R/config.R`, which ships `FALSE` until a person
confirms the notice actually renders. A fifth lineage needs a fifth constant,
defaulting to `FALSE`, not a hardcoded argument.

The obligation attaches to *using* the data, not to the drawing succeeding: the
abr backlink renders whether or not the map above it managed to draw.
