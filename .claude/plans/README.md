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
| `deck-compare/plan.json` | The authoritative plan for Deck Compare (the ice/breaker app's NetrunnerDB decklist fetch and pairing feature). 5 milestones, decisions DL-031..DL-040. |
| `deck-compare/context.json` | The context the plan was built from: task spec, constraints, entry points, rejected alternatives. |
| `deck-compare/qr-plan-design.json` | The quality-review items the plan was verified against. |
| `deck-compare/architect-findings.md` | Findings from the plan-design QR remediation pass. |

## Decision ids

`DL-001` through `DL-010` belong to the original implementation plan in
the homelab repo (`docs/netrunneR/netrunneR-implementation-plan.md`), and
are referenced from the compose and systemd files. The cost-model plan
continues from `DL-011` rather than restarting, so an id means one thing
across both repositories.

`DL-031` through `DL-040` belong to Deck Compare (the ice/breaker app's
NetrunnerDB decklist fetch and pairing feature: `R/fetch-deck.R`,
`R/deck-compare.R`, the `all_codes` addition to `load_ice_breaker_app_data()`
in `R/operations.R`, the shared render helpers extracted from
`R/mod_matchup_explorer.R`, and the nav-destination view module wired in
milestone M-005). That plan continues from `DL-030` rather than restarting,
for the same reason the cost-model plan continues from `DL-011`. Its
planner state lives at `deck-compare/`, in this same directory, moved here
from a Windows workstation's temp directory rather than left there --
that was itself a gap this ledger had for a while: it discussed this
plan's decisions in more depth than any other entry while the plan.json
they came from existed nowhere durable.

**A real collision happened here, corrected twice.** The abr/cobra
cross-reference plan (Phase 1 of the phased design at
`docs/netrunneR/plans/2026-09-03-abr-cobra-xref/plan.md` in the homelab repo)
was planned concurrently with Deck Compare, in a separate session, and
independently computed the same "next available" starting point from this
same file -- landing its own Phase 1 decision (`R/build-cobra.R`'s
`abr_code`-is-a-plain-reference call) at `DL-031`/`DL-032`, the same numbers
Deck Compare had already claimed. `DL-032` was live in committed code under
both meanings at once before this was caught.

First correction moved cobra's pair to `DL-040`/`DL-041`, checked only
against Deck Compare's LANDED code at the time (`DL-031`-`DL-039` across
`R/fetch-deck.R`, `R/deck-compare.R`, `R/operations.R`,
`R/mod_matchup_explorer.R`, `R/README.md`), not against the FULL plan,
which already specified milestone M-005 (not yet built) citing `DL-040`
for its own decision. That created the exact same class of collision a
second time, this time self-inflicted, caught before M-005 was built by
reading the plan's complete decision list rather than only its landed
footprint.

Second correction moves cobra's pair again, to `DL-042`/`DL-043`, past
Deck Compare's true high-water mark including its unbuilt milestone.
`DL-041` is left as a deliberate gap rather than reused, so cobra's two
decisions stay a contiguous, easily-cited pair. `man/COBRA_TOURNAMENT_ALLOWLIST.Rd`
regenerated to match on both corrections.

**The lesson, for whichever session reads this next:** compute "next
available id" against a plan's full decision list (including unbuilt
milestones), never only its landed code. A plan that reserves ids ahead of
where it has built to is the normal case, not an edge case.

The high-water mark was `DL-043`. Any future abr/cobra Phase 2 work
reserves headroom starting at `DL-044`, not from the old `DL-033`-`DL-038`
range this entry originally (wrongly) claimed for it -- that range was
never actually used by Phase 1 and belongs to Deck Compare instead.

`DL-045` is a post-landing fix to Deck Compare itself, not part of the
original `DL-031`..`DL-040` plan: `resolve_deck_codes()` was reading a
`deck$identity` field that the live NetrunnerDB `/decklist/<id>` envelope
has never actually returned (the same gap R/README.md's decklist-mirroring
postmortem had already named). Fixed by finding the identity via
`type_code == "identity"` against the resolved cardpool codes instead.
`DL-044` stays reserved for abr/cobra; the high-water mark is now `DL-045`.

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
