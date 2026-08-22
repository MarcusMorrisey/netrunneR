# netrunneR 0.1.0

* Initial offline-mirror implementation: nrdb, abr, cardpool, rules and
  implementation lineages, shared sync/promote/validate/ledger machinery,
  and derived ratings/matchup views.
* cobra and assets lineages are deferred: no code ships for them in this
  release; the new_lineage() extension point exists now and does not
  require either lineage to be implemented.
