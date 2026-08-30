# netrunneR M-001 — break-side and ice-side cost extraction

You are implementing **M-001**, the first of three milestones in step 3 of
the ice/breaker matchup-cost work. Steps 1 and 2 are merged; a mitigation
that M-001 supersedes is merged too. Read this whole document before
touching anything — several of the obvious moves are traps that have
already been hit once.

The plan is at `C:\Users\marcu\AppData\Local\Temp\planner-ih7losns\plan.json`.
It is authoritative for decisions (DL-011..DL-030), risks and acceptance
criteria. This document is orientation, not a replacement for it.

---

## Where the code is, and how to run anything

`marcus@mediaserver:~/src/netrunneR` **over SSH. There is no local clone.**
`master` currently at `86f16fc`. The repo merges with merge commits:
`gh pr merge N --merge --delete-branch`, base `master`.

The authoritative cost source is the pinned implementation release:

```
/srv/netrunner-mirror/data/implementation/releases/\
a395d44c13fba31a46737336f94c0b483f9b1bb6-bad32763e17e7/raw/src/clj/game/cards/
  programs.clj   # breakers
  ice.clj        # ice, and the Corp-side :additional-cost
```

**There is no R on the host.** Everything runs in the
`netrunner-mirror-sync:latest` image, whose ENTRYPOINT must be overridden:

```bash
ssh marcus@mediaserver 'cd ~/src/netrunneR && docker run --rm \
  --entrypoint Rscript -e RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
  -v $PWD:/pkg -w /pkg netrunner-mirror-sync:latest --vanilla \
  -e "pkgload::load_all(\".\", quiet=TRUE); testthat::test_dir(\"tests/testthat\")"'
```

**753 tests pass on `master`.** For anything touching real data, add
`-v /srv/netrunner-mirror/data:/data:ro`.

**roxygen2 is not in the image.** It is in `/tmp/rlib`; mount it with
`-v /tmp/rlib:/rlib` and use
`.libPaths(c("/rlib", .libPaths())); roxygen2::roxygenise("/pkg")`.
Run it after any `@export` change: `pkgload::load_all()` exposes
unexported functions, so the suite passes while the installed package
fails to boot. This has already happened once in this project.

### The quoting trap — you will hit this

Apostrophes in roxygen comments (`#'`) terminate a single-quoted `ssh`
argument, and backslashes in unquoted heredocs get eaten by the remote
shell, silently turning `\uXXXX` into a literal character.

**Write every edit as a Python script locally, `scp` it up, and run it.**
Do not use inline heredocs for anything containing R source. Use
`chr(92)` to build a backslash rather than typing one. Each script should
`assert` its anchor matches exactly once before writing, so a failed
patch fails loudly instead of silently doing nothing.

---

## What is already merged — do not redo it

| PR | What it did |
| --- | --- |
| **#26** | `pump_economics()` reads `strength-pump` cost forms. 11 breakers were recorded as fixed-strength when they have a pump; every stealth breaker was reporting it could not reach ice it reaches routinely. Added `pump_stealth`, `pump_resource_type`, `pump_resource_qty` to traits and `stealth_credits` to the matchup table. `spec_id` → v3. |
| **#27** | Split absent matchup rows into `CANNOT BREAK` (subtype filter excluded on purpose, overridable per lane) vs `NOT COMPUTABLE`. Added `matchup_pair_state()`, `assumed_pair_cost()`, `override_control_ui()`. |
| **#28** | `break_sub_subtype()` now takes the **first** literal (the subtype is the third positional argument) — fixed Endless Hunger, which recorded `" subroutine"`. Added `break_subtype_count` and a guard so the board does not claim `CANNOT BREAK` on partial evidence. |

**#28 is explicitly a guard, not a fix, and M-001 owns the fix.** Its
comments say so. When every clause is recorded, the `count > 1` guard in
`matchup_pair_state()` comes out and `break_subtype_count` becomes a
value derived from the recorded clauses (DL-029, and there is an
acceptance criterion asserting the guard is gone).

---

## The work

### 1. Read break-sub costs instead of discarding them

`breaker_economics()` in `R/build-implementation.R` matches only
`(break-sub <int> <int> "Subtype")`. Everything else sets
`parse_status = "non_credit_break_cost"` and **throws the cost away** —
17 cards.

Use one shared `parse_cost_form()` for both the pump side and the break
side (**DL-011**). `pump_economics()` is the working model; do not write a
second reader, because two readers that disagree about the grammar is
precisely what caused the Endless Hunger bug (#28).

The cost taxonomy, extracted from source rather than recalled:

| type | notes |
| --- | --- |
| `:credit` | optionally `{:stealth 1}` or `{:stealth :all-stealth}` |
| `:power`, `:virus`, `:any-virus-counter` | counters |
| `:trash-can`, `:trash-from-hand`, `:trash-installed` | trashing |

**Stealth credits are credits** — the qualifier says where they are
sourced, not that they are a different currency — so they count in the
credit total with the constraint annotated separately (**DL-014**).
Counters and trashing are **not** credits and must never be summed into
one (**DL-012**); they are a typed `(resource, quantity)` pair.

Two measured facts you can rely on without re-deriving (**DL-023**,
**DL-024**):

- Max **1** distinct non-credit resource per cost vector, and **0** cost
  vectors contain more than one `->c` form. The reader must still assert
  this and emit `multi_resource_cost` rather than silently keeping the
  first if it ever stops being true.
- **Quantity-less forms exist.** `(->c :trash-can)` has no number and
  means an implicit 1; four break costs use exactly `[(->c :trash-can)]`.
  **The pump reader's pattern requires a digit and would skip all four.**
  Reusing it verbatim is the trap. No `strength-pump` cost is
  quantity-less, so #26 is correct as it stands.

### 2. Variable costs, and the predicate that matters

`(break-sub nil ...)` is **not** free and **not** a parse failure. All
four sit inside an X-credit payment
(`(cost-value eid :x-credits)`), so the cost is fixed at play time —
the break-side twin of the existing `variable_subroutines` (**DL-021**).

The four sites: Lobisomem `:2059`, Matryoshka `:2181`, Paperclip `:2569`,
Utae `:3587`. `programs.clj:164` is inside the `pump-and-break` helper
with `subtype` as a parameter and has **no enclosing defcard** — a
definition site, not a fifth card (**DL-022**).

**A card is a variable-cost breaker only when it has NO fixed break
clause** (**DL-028**). Only Paperclip and Matryoshka qualify outright.
"Has a variable clause" and "is a variable-cost breaker" are different
predicates; conflating them re-introduces a wrong answer.

Lobisomem and Utae each carry a fixed clause as well, but they are not
the same case and only one of them is settled:

- **Lobisomem** keeps its fixed cost. Its two clauses name *different*
  subtypes (Code Gate fixed, Barrier variable), so the subtype of the ice
  in the pair picks the clause and no tie-break is needed.
- **Utae** is NOT settled. Both its clauses name Code Gate, so the
  subtype key cannot choose between them, and its fixed clause is the
  *conditional* one. Whether Utae keeps a fixed cost or becomes
  `variable_break_cost` is the open decision below — **do not assume
  either answer while reading this section.**

### 3. Helper call sites carry literal costs — do not skip them

`defn-` bodies are definition sites and are skipped, but the **call
sites** pass literals and must be read (**DL-022**):

- `central-only` receives a whole form: `(central-only (break-sub 1 1 "Sentry") ...)` — Alias, Breach, Passport
- `pump-and-break` receives the cost positionally: `(pump-and-break [(->c :credit 3)] 2 "Code Gate")` — Black Orchestra, MKUltra

Treating these as out of scope would abandon five breakers whose costs
are in plain sight. (An earlier instruction to skip them was wrong and
was corrected.)

### 4. Every clause, not one per card — the real fix

This is the substantial part, and it changes the table's shape.
`ice_breaker_traits` records one break clause per card. Some cards have
several, with **different subtypes and different costs**:

- **Penrose** — Code Gate, plus Barrier the turn it is installed. We record Barrier; **133** Code Gate pairings are absent.
- **Lobisomem** — Code Gate for 1 credit, plus X Barrier subroutines. We record Code Gate; **101** Barrier pairings are absent.

Record every clause with its own subtype and cost, and make
`breaker_matches_ice()` test **any** recorded clause (**DL-029**). That
emits the 234 missing pairings.

Same-subtype multi-clause cards are fine as they are: BlacKat, Euler,
Odore, Revolver. **Utae has two clauses, not three** — the
`(break-sub 1 1 "Code Gate")` at `:3568` is bound in a `let` purely to
read its `:break-req`.

### 5. Corp-side additional rez costs

Five ice in `ice.clj` carry `:additional-cost` — `:tag-or-bad-pub` (2),
`:forfeit` (2), `:derez-other-harmonic` (1). Record type and quantity on
the ice side. These are **not credits** and never enter
`credit_differential` (**DL-015**); they surface as a Corp-side chip in
M-003. Every other ice records none.

---

## OPEN DECISION — settle this with the user before writing code

**DL-030's tie-break is wrong as written, and must not be implemented
until the user rules on it.**

It says a fixed clause outranks a variable one when both match the
subtype. Read Utae, the only card where the tie-break actually fires:

```clojure
;; the VARIABLE clause -- the general one, requires only strength
:cost [(->c :x-credits)]  :once :per-run
:break-req (has-subtype? current-ice "Code Gate")

;; the FIXED clause -- CONDITIONAL on board state
(break-sub 1 1 "Code Gate"
  {:req (req (<= 3 (count (filter #(has-subtype? % "Virtual")
                                  (all-active-installed state :runner)))))})
```

Utae's fixed clause needs **three or more installed Virtual resources**.
"Fixed wins" therefore prices Utae at 1 credit per subroutine — a cost
available only in a particular board state — and presents it
unconditionally. That is the Penrose mistake in mirror image: preferring
a conditional ability over the general one.

It also contradicts the documented scope of `cost_to_break`, which is
"the standard encounter, not the cheapest line of play" and explicitly
does not model card-specific tricks.

The tie-break only fires when two clauses share a subtype, which today is
Utae alone. Penrose is resolved by the subtype key (Barrier pair →
Barrier clause), so it does not depend on this at all.

**Candidate rules, for the user to choose:**

1. **Prefer the unconditional clause** (no `:req`). Utae becomes
   `variable_break_cost`. Loses a number, but the number it shows today
   is conditional and unlabelled. Where the only clause for a subtype is
   conditional — Penrose against Barriers — it is still priced, as now.
2. **Prefer unconditional, and mark conditionally-priced pairs** with a
   status so the condition is visible rather than silent. More honest,
   more scope, and it needs a `:req` reader.
3. **Fixed wins** (DL-030 as written) — now known to be wrong for Utae.

Do not pick one unilaterally. Whichever is chosen, amend DL-030 and
record the reasoning at the point of decision.

## The coupling you must settle before writing code

**DL-030** puts clause selection in the cost model at pair-pricing time,
keyed by the ice subtype that made the pair compatible.

That decision spans M-001 and M-002: M-001 produces the clauses, M-002
chooses between them. Lobisomem breaks a Code Gate for 1 credit and
Barrier subroutines for X, so a pair's cost depends on **which clause
applies**. Settle the representation — a `break_clause` child table
keyed by code, or an encoding within `ice_breaker_traits` — before
writing either side, and record the reasoning at the point of decision.

Also note `assumed_pair_cost()` in `R/mod_lane_board.R` re-implements the
formula call for overridden pairs. Any signature change must be mirrored
there or overridden pairs silently price differently from ordinary ones
(**R-002**).

---

## Conventions that are not optional here

- **Comments explain why, especially why something is NOT done.** This
  codebase documents rejected alternatives at the point of the decision.
  Match that register or the file reads as foreign.
- **Honesty over coverage.** Unknown stays `NA` with a status naming the
  reason (**DL-016**). Never guess a number that looks plausible. The
  four answers — "cannot", "expensive", "variable by design", "we could
  not read it" — must never collapse into each other.
- **Tests use card bodies copied verbatim** from the mtgred/netrunner
  tree, never paraphrased fixtures. The shapes this code has failed on
  were exactly the ones nobody thought to imagine.
- **No non-ASCII in R source** outside comments; use `\uXXXX` escapes.
  (`R/mod_lane_board.R:311` has a pre-existing literal `×` — a separate
  task exists for it; do not fold it in.)
- **Pre-ship gates**: constants in `R/config.R`, `require_*()` guards in
  `R/app.R`, and `test-preship-gates.R` asserts an open gate means the
  notice actually renders. Never pass a literal `TRUE` to a guard.
  Opening one is a human attestation — ask, do not decide.
- **Schema tolerance**: a release built before a column exists must load,
  not abort. `fill_missing_columns()` is the established pattern, and
  there is an acceptance criterion for it.

---

## Definition of done

Work `master` → feature branch → PR. Do not merge; report and let the
user decide. The acceptance criteria in `plan.json` under M-001 are
authoritative; the load-bearing ones:

- No trait row carries `non_credit_break_cost`; each of the 17 resolves
  to a priced cost, `variable_break_cost`, or a named unreadable status,
  with per-status counts matching a source census.
- Switchblade, Audrey v2, Faust, Propeller, Hantu record real costs;
  Alias, Breach, Passport, Black Orchestra, MKUltra read from their
  helper call sites; the four `[(->c :trash-can)]` costs record
  quantity 1.
- Paperclip and Matryoshka record `variable_break_cost`; Lobisomem keeps
  its fixed Code Gate cost. **Utae's outcome depends on the open decision
  above and has no fixed expectation here** — whichever rule is chosen,
  its acceptance criterion is written to match, not the reverse.
- Penrose and Lobisomem record both clauses and the 234 pairings are
  emitted.
- `matchup_pair_state()` contains no `count > 1` guard, and a breaker
  with complete evidence still reports `cannot_break`.
- Endless Hunger keeps the subtype #28 gave it; **no breaker loses a
  pairing it emits today**.
- `programs.clj:164` contributes no trait row.
- roxygen regenerated, NAMESPACE current, package loads from an
  installed build and not only under `load_all()`.

---

## One thing that will not be true on merge

**None of this goes live when the PR merges.** Traits are built by the
sync container, which is built from a **pinned package SHA** — not from a
working tree. It takes effect only after the image is rebuilt at a SHA
containing the change and the implementation lineage is re-synced:

```bash
cd ~/src/homelab/compose/netrunner-mirror
LINEAGE=implementation MODE=scheduled docker compose run --rm sync
```

**#26 and #28 are already waiting on that same re-sync, and M-001 changes
the schema again.** One image rebuild should cover all three. Rebuilding
and promoting is a production action on the live mirror — **do not do it;
surface it to the user.**

The app meanwhile serves the old traits correctly and without erroring,
because of `fill_missing_columns()`. Verify that stays true: load
`load_ice_breaker_app_data()` against the currently-promoted release and
confirm it still returns 21,377 rows.
