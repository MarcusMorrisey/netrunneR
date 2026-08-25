# netrunneR (development)

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
