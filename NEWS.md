# netrunneR (development)

* The lane board distinguishes "cannot break" from "not computable". A
  Fracter stacked under a Code Gate produced no matchup row, and an
  absent row rendered as `not_computable` -- which says we do not know,
  when the subtype filter dropped that pair ON PURPOSE and it is the most
  definite statement the app can make about a pairing. A breaker whose
  break clause could not be read still reads `not computable`, because
  there we genuinely do not know what it breaks.
* An incompatible pairing can be overridden per lane, with a checkbox.
  Subtypes are not immutable during a game -- effects add them to ice --
  so the operator can assert that this ice, here, is breakable. The
  arithmetic that unlocks is badged `ASSUMED`, never `FORMULA`: the
  numbers are ours and the premise is theirs. The override is
  session-only and lane-specific, and is deliberately NOT written to
  `matchup_overrides.csv`, which is for corrections true of the cards
  themselves rather than of one board state.
* `mod_lane_board_server()` takes an optional `traits`. Without it both
  absent-row cases collapse back to `not computable`, which is what the
  board said before it could tell them apart.

* `mod_matchup_explorer` is reachable again. It had been exported, tested
  and mounted nowhere since the lane board replaced the navbar that
  hosted it. It is now a modal opened from the card detail modal, so no
  navigation is re-introduced: you arrive with a card, and its type
  decides which side of the pair is being asked about. The mode selector
  and both card pickers are gone as a consequence, as is the "All vs all"
  mode. `mod_matchup_explorer_ui()` is removed, not deprecated -- the
  view is built inside the modal by the server.
* The matchup view filters by format, defaulting to Standard, matching
  `mod_card_browser`. Both sides of a pair must be legal, so a Standard
  breaker is not scored against rotated ice. Filtering happens at
  display: `compute_ice_breaker_matchups()` stays format-blind, and the
  lane board still has no format control -- the same gap, still open, in
  a live view.
* The matchup empty state says WHY it is empty. A breaker whose break
  clause the implementation parser could not read (36 of 173 icebreakers
  in the current pool) is reported as a gap in our data rather than as a
  card that breaks nothing.
* `load_ice_breaker_app_data()` also returns `traits`, which is what
  makes the distinction above possible.
* `mod_card_detail_server()` takes an optional `on_compare` callback.
  NULL omits the control rather than rendering one that does nothing.
* Fix a latent modal bug: the `hidden.bs.modal` dismissal handlers are
  delegated on `document` and fired for any modal, so a second modal
  would have cleared the first module's `reactiveVal` mid-transition.
  Each modal now carries a marker div its own handler checks.

* Add the ice-vs-breaker matchup Shiny app: `mod_card_browser`,
  `mod_matchup_explorer`, `mod_card_detail` modules, wired together by a
  new `inst/shiny-app/app.R` (the entry point `shinyAppDir()` actually
  needs -- it did not exist before, so `run_app()` could not previously
  find an app to serve).
* `compute_ice_breaker_matchups()` now takes a required `matchup_overrides`
  argument (manually-curated corrections, pair-keyed, shipped as
  `inst/extdata/matchup_overrides.csv`) and emits `source`
  (`"formula"`/`"override"`/`"not_computable"`) and `credit_differential`
  columns. `cost_to_break` is still `NA` for every non-override pair,
  pending real per-card trait extraction from the mtgred/netrunner
  Clojure source (`extract_ice_breaker_traits()` remains a stub) -- this
  is the honest current state, not a regression.
* Add `card_image_url()`, verified against NetrunnerDB's own public API
  response (`imageUrlTemplate` field) and documented API purpose; covered
  by the existing `require_cardpool_disclaimer()`, not a new guard.
* Add `safe_render()` so a Shiny render error degrades to an inline
  message instead of Shiny's default per-output error banner.

# netrunneR 0.1.0

* Initial offline-mirror implementation: nrdb, abr, cardpool, rules and
  implementation lineages, shared sync/promote/validate/ledger machinery,
  and derived ratings/matchup views.
* cobra and assets lineages are deferred: no code ships for them in this
  release; the new_lineage() extension point exists now and does not
  require either lineage to be implemented.
