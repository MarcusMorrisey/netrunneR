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
