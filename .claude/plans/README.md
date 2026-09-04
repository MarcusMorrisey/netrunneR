# Plans

Planning artefacts that outlive the session that produced them.

## Why these are here

The planner writes its state to a temp directory. That is fine while a
plan is being made and useless the moment the machine sweeps `%TEMP%` --
which would have taken `plan.json` with it, along with every decision
recorded in it and the acceptance criteria the work is checked against.

`.claude` is already in `.Rbuildignore`, so nothing here reaches the
built package.

## What is here

| Path | What |
| --- | --- |
| `M-001-break-cost-extraction.md` | Starting prompt for M-001, the first milestone of the cost-model work. Self-contained: toolchain, conventions, what is already merged, definition of done. |
| `step-3-cost-model/plan.json` | The authoritative plan. 3 milestones, 20 decisions (DL-011..DL-030), 10 risks, per-card acceptance criteria. |
| `step-3-cost-model/context.json` | The context the plan was built from: task spec, constraints, entry points, rejected alternatives, invisible knowledge. |
| `step-3-cost-model/qr-plan-design.json` | The quality-review items the plan was verified against. |

## Decision ids

`DL-001` through `DL-010` belong to the original implementation plan in
the homelab repo (`docs/netrunneR/netrunneR-implementation-plan.md`), and
are referenced from the compose and systemd files. The cost-model plan
continues from `DL-011` rather than restarting, so an id means one thing
across both repositories.

`DL-031` through `DL-039` belong to Deck Compare (the ice/breaker app's
NetrunnerDB decklist fetch and pairing feature: `R/fetch-deck.R`,
`R/deck-compare.R`, the `all_codes` addition to `load_ice_breaker_app_data()`
in `R/operations.R`, and the shared render helpers extracted from
`R/mod_matchup_explorer.R`). That plan continues from `DL-030` rather than
restarting, for the same reason the cost-model plan continues from
`DL-011`.

**A real collision happened here, corrected rather than merely renumbered
away.** The abr/cobra cross-reference plan (Phase 1 of the phased design at
`docs/netrunneR/plans/2026-09-03-abr-cobra-xref/plan.md` in the homelab repo)
was planned concurrently with Deck Compare, in a separate session, and
independently computed the same "next available" starting point from this
same file -- landing its own Phase 1 decision (`R/build-cobra.R`'s
`abr_code`-is-a-plain-reference call) at `DL-031`/`DL-032`, the same numbers
Deck Compare had already claimed for two of its own decisions. `DL-032`
specifically was live in committed code under both meanings at once before
this was caught: `R/build-cobra.R:30` and `R/deck-compare.R`/`R/README.md`
each citing the same id for two unrelated decisions.

Resolved by renumbering the smaller, more recently landed footprint: the
abr/cobra Phase 1 decision moved from `DL-031, DL-032` to `DL-040, DL-041`
in `R/build-cobra.R` (regenerated `man/COBRA_TOURNAMENT_ALLOWLIST.Rd`
alongside it). Deck Compare's citations across `R/fetch-deck.R`,
`R/deck-compare.R`, `R/operations.R`, `R/mod_matchup_explorer.R` and
`R/README.md` were left untouched, since renumbering them would have meant
editing a footprint already spread across five files plus documentation
that had just been synced, against one that was one file, one line.

The high-water mark is now `DL-041`. Any future abr/cobra Phase 2 work
reserves headroom starting there, not from the old `DL-033`-`DL-038` range
this entry used to (wrongly) claim for it -- that range was never actually
used by Phase 1 and now belongs to Deck Compare instead.

## One decision is deliberately unmade

`DL-030` -- how to choose between two break clauses that name the same
subtype -- is recorded as written but is **known to be wrong for Utae**,
whose fixed clause is conditional on three or more installed Virtual
resources while its variable clause is the general one. Preferring the
fixed clause would price a board-state-specific cost as the standard
encounter.

It is left unamended on purpose: amending it IS the decision, and it is a
human one. The M-001 prompt states the three candidate rules and
instructs whoever picks it up not to choose unilaterally.

## Not preserved, and why

The planner left 55 state directories in temp. All but three held only an
empty 352-byte scaffold from a run that never produced a plan. Of the
three:

- the large Aug 22 plan is an OLDER copy of
  `homelab/docs/netrunneR/netrunneR-implementation-plan.md` -- it still
  describes the pre-consolidation `/srv/<lineage>-data` store layout,
  which homelab's copy supersedes
- an orphaned Aug 22 context describes upstream API fixes that shipped
  long ago, and its own plan was never written

Neither adds anything the repositories do not already hold.
