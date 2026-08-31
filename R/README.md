# R/ Architecture and Design Decisions

## Overview

This directory holds the whole netrunneR package: five external Netrunner
data sources ("lineages") mirrored into local SQLite-backed release stores
with atomic promote semantics. All five run the same sync pipeline; only
fetching and building vary, and both vary by S3 dispatch on the lineage's
`source_type` rather than by branching inside the pipeline.

The directory also holds two things that are NOT part of the mirror: the
`views-*` files, which derive ratings, ice/breaker matchups and tournament-meta
figures from whatever release happens to be active, and the `mod_*` files, which
are the Shiny modules that render them. They are consumers of the pipeline, not
stages in it -- no view has a lineage, a release or a store, and none of them
can make one. `compute_ice_breaker_matchups()` and `compute_identity_ratings()`
are plain functions called live; `matchup` and `ratings` are deliberately not
members of `BUILTIN_LINEAGES`.

That boundary is why the view layer's dependencies are Suggests rather than
Imports: the sync container runs the pipeline and draws nothing, so making it
carry a spatial stack to load the package at all would be paying for a view it
never opens.

Reading the source tells you what each function does. This file records
what it cannot: why the abstraction is shaped this way, which upstream
realities forced which design, and which invariants are load-bearing but
not enforced by the language.

## Architecture

### The lineage abstraction

`.LINEAGE_REGISTRY` in `lineage.R` is the single source of truth for the
five built-in lineage names and their static configuration: schedule,
schema version, pacing, build module path, plus per-type extras
(`repo_url`/`ref` for git-mirror lineages, `hub_url` for the web-archive
lineage). `lineage(name)` resolves a name into an S3 object classed
`c("netrunneR_<source_type>", "netrunneR_lineage")`.

There are three source types, and each has exactly one
`fetch_lineage.<class>()` method:

| Source type   | Lineages                | Fetch mechanism                                          |
| ------------- | ----------------------- | -------------------------------------------------------- |
| `api_poll`    | nrdb, abr               | HTTP with throttle pacing; 5xx handling differs by lineage (below) |
| `git_mirror`  | cardpool, implementation | `gert::git_clone()` plus checkout, no pacing              |
| `web_archive` | rules                   | Scrape an HTML index, pool PDFs by content hash           |

Within `api_poll`, 5xx handling is not uniform across the two lineages.
`abr_get()` (`R/fetch-abr.R`) disables httr2's default error handling,
checks `resp_status(resp) >= 500` explicitly, and aborts immediately
with a custom `netrunneR_abr_5xx` condition, protecting a volunteer-run
server from a retry storm. `nrdb_get()` (`R/fetch-nrdb.R`) has no
equivalent: it only adds `httr2::req_retry(max_tries = 5)` on top of
httr2's default behavior, which retries transient/429/503 statuses and
otherwise raises httr2's ordinary generic error on any other non-2xx
status, including most 5xx.

Git-mirror lineages carry no pacing deliberately: the upstream is GitHub,
not a volunteer-run community server, so the courtesy throttle that
protects `alwaysberunning.net` would only slow the clone down.

Two lineages can share a class and a fetch method while needing entirely
different build logic. That is why `build_lineage.netrunneR_api_poll()`
and `build_lineage.netrunneR_git_mirror()` re-dispatch internally by
lineage name rather than being one method per lineage: the fetch shape is
what the class captures, and the schema is not.

### Store roots are container-side paths, always

Every lineage's `store_root` resolves to `file.path("/data", name)`. That
is a container-side path, byte-identical on both sides of the Docker bind
mount; the host-side mapping lives in deployment configuration outside
this package and is never named in R code. The consequence is that the
package behaves identically whether it runs inside the sync container or
directly against a host filesystem, and a host storage reorganization
requires zero R changes. This is enforced, not merely intended: a static
source scan in the test suite asserts that no `/srv` literal appears
anywhere under `R/`.

### The eight-step sync pipeline

`run_sync()` in `sync.R` executes the same sequence for every lineage:

1. Acquire the per-lineage `filelock` lock (non-blocking) and check space.
2. Dispatch `fetch_lineage()` into a fresh `staging/<attempt_id>`.
3. No-op check: compare `content_identity` and `build_revision` against
   the active manifest and short-circuit to a `no_change` ledger record
   on a match.
4. Dispatch `build_lineage()`, writing SQLite via the lineage's build
   module.
5. `validate_release()`.
6. Stage the manifest.
7. Promote: `fs::file_move()` the staged directory into
   `releases/<release_id>`, then swap the `active` symlink.
8. Append an ND-JSON ledger record, fsynced before returning.

Promotion is atomic because it is a same-filesystem `rename(2)`. This is
why the store must be a direct bind mount and never a named Docker
volume: a storage driver interposed on the mount does not guarantee
same-filesystem rename semantics, and atomic promotion is the entire
safety property of the design.

Lock contention is a successful skip, not a failure. `run_sync()` signals
a typed `netrunneR_lock_contention` condition rather than terminating;
only the CLI wrapper turns that into a process exit status, and no
function under `R/` calls `quit()`.

### Release identity

A `release_id` is composite: `source_revision` (from the fetch, e.g. a git
commit SHA or a timestamp) plus `build_revision` (a digest over the build
module, every shared build and write module, every file under
`inst/sql/schema`, the lineage's schema-version constant and the
`renv.lock` hash).

The composite is what makes the no-op check honest in both directions:

- Same source, same build code, means no new release.
- Same source, changed build module or DDL, means a new, distinct,
  non-colliding release even though nothing upstream moved.

`build_revision` is computed identically for all five lineages rather than
only for git-mirror lineages, so a change to shared code or shared DDL
forces a rebuild everywhere it could affect output. Narrowing it would let
a shared-code change silently leave stale releases in place.

## Design Decisions

### ABR outage detection distinguishes fresh failures from retries

`run_abr_backfill()` originally kept one blind counter: N consecutive 5xx
responses meant a live upstream outage, so abort. That counter could not
tell a fresh tournament id failing for the first time from a retry of an
id already known bad. Because persistently-broken tournaments get retried
together as a small batch, three of them failing back-to-back was
indistinguishable from a genuine outage, and the backfill would abort
forever.

The fix snapshots the known-failing ids before the loop. Only fresh,
never-before-seen failures feed the outage counter; retries of
already-flagged ids fall through to the per-id tombstone path unchanged.
Real outage protection survives, and a handful of permanently broken
tournaments no longer blocks the whole crawl.

### Tombstones, not placeholder rows

Every ABR tournament id gets a checkpoint row carrying `resolved`,
`first_failed_at`, `last_failed_at` and `permanent_unavailable`. A
persistently failing id is retried daily for `ABR_BACKFILL_TOMBSTONE_DAYS`
before being marked permanently unavailable, at which point `fetch_abr()`
excludes it from the tournament table entirely rather than writing a
placeholder. A partial row would be indistinguishable from real data
downstream; an absent row is honest.

This path is live-exercised, not theoretical. Five ABR tournament ids
return genuine, persistent HTTP 500 from an otherwise healthy API
(confirmed by direct `curl`). Rather than wait out the full tombstone
window, those ids were fast-forwarded to `permanent_unavailable` in the
checkpoint directly, accepting them as permanently lost.

### The cardinality check accounts for legitimate exclusions

`tournament_id_cardinality` originally compared `nrow(tournaments)` (after
permanent exclusions) against the raw upstream `tournament_count` (before
them), so it failed the moment any tournament was ever tombstoned. The fix
threads `permanent_ids` through `fetch_abr()`'s return value so
`build_abr()` computes the correctly adjusted expected count. The check
still fails on any other mismatch, such as duplicates or pagination bugs:
the fix narrows it rather than gutting it.

### The personal-data defense guards a path the risky data never takes

ABR tournament data passes through a fail-closed named `dplyr::select()`
allowlist before reaching `dbWriteTable()`, and `check_deny_pattern()`
independently scans column names for personal-data-shaped terms before
every write. Two independent layers, deliberately.

Verification against the real API found that the `/entries` endpoint's
`user_name` and `user_import_name` fields genuinely are personal data
(real player names), but that data never flows through `build_abr()`'s
allowlist or `dbWriteTable()` path at all: `fetch_abr()` and
`run_abr_backfill()` write it straight to raw JSON or the
content-addressed object pool. The deny-pattern layer therefore currently
guards a path that entries data never reaches. It was left unchanged
rather than extended, because there was nothing to extend it to, and
removing a fail-closed defense because it is currently redundant is how
it stops being there when the data flow changes.

### NRDB envelope: an honest shape check, not a fabricated comparison

Earlier code assumed the NetrunnerDB response carried `total`,
`last_updated` and `version_number` fields and compared them against the
previous release. The real response is a bare `{"data": [...]}` array:
none of those fields exist, and on inspection they were never being
persisted to the manifest anyway, so the comparison had always been
silently inert.

They were replaced with `compare_shape()`, which checks only that the
response has a `data` list. Actual content-change detection for nrdb
happens where it happens for every other lineage, in `no_op_change()`'s
`content_identity` diff at the sync layer, not inside the fetch.

### Decklist mirroring was removed, not repaired

`decklist-sweep.R`, the ABR-to-decklist reconciliation and tombstone
logic, and the `decklist` table in `nrdb.sql` were deleted outright. The
feature had been built against assumptions never checked against the real
API: the wrong date field, an `identity_code` with no upstream source at
all, and an unserializable nested `cards` list-column that reached
`dbWriteTable()`. Decklists are not needed for this mirror. `nrdb` now
mirrors reviews and rulings only. This is a closed decision, not a gap to
fill back in.

### Rules version ordering: trust the hub, warn on disagreement

`check_version_monotonic()` had two real bugs and prompted one design
call. It compared the hub's newest-first listing order against an
ascending date sort, which is backwards, so it failed on every correctly
ordered real index; that is fixed to sort descending.

The remaining problem is that `published_date` comes from each PDF's HTTP
`Last-Modified` header, because the hub page carries no real publish-date
field, and that header genuinely drifts from true release order: the
upstream has re-uploaded old PDFs without any content reorder, so an older
version's header can postdate newer ones. The decision is to treat the
hub's own listing order as authoritative, since that order is what gets
written and promoted either way, and to downgrade a disagreement from a
blocking failure to a warning. Blocking promotion forever because one old
PDF's mtime drifted is not useful signal.

### The rules hub has no tables

An earlier scraper selected `table.rules-index`. The real page contains
zero `<table>` elements. Its actual structure:

- The latest version lives in a Kadence `div.wp-block-kadence-infobox`
  block with a canonical PDF link, an `_Annotated` sibling variant, and a
  non-PDF "(Web)" link.
- Older releases are plain `<p><a href="...vXX.XX....pdf">` links,
  including older `NISEI Comprehensive Rules v1.X` entries.
- A separate "Card Text Updates" section carries its own PDF links that
  are a different document category and must be excluded.

Two exclusion details are easy to get wrong and worth stating: the
annotated variant is excluded by matching its `href`, not its link text,
because that link reads "PDF with Changes Highlighted" and text-based
filtering lets it straight through; and the "(Web)" link needs no special
handling because selecting only `a[href$=".pdf"]` already drops it. The
version regex `v\.?(\d+\.\d+)` (case-insensitive) covers the real variants
observed upstream: `v26.03`, `v.25.04`, `v1.6`.

## Invariants

These hold across the package and are not enforced by the compiler.

- **No host paths in R source.** R code must never construct or reference
  a host filesystem path. The container-side `/data/<name>` mount point is
  the only path any lineage's code sees. A static source scan in the test
  suite fails the build if a `/srv` literal appears under `R/`.
- **Bind mount, never a named volume.** Atomic promotion depends on
  `rename(2)` within one filesystem. A named Docker volume does not
  guarantee that.
- **One byte-capture boundary.** `capture_response_body()` is the only
  function permitted to move httr2 response bytes to disk. No
  `fetch_lineage()` method may write a response or its headers directly,
  so headers, cookies and the response object fall out of scope
  immediately. Enforced by a static AST scan plus a behavioral
  sentinel-header test.
- **Allowlists fail closed.** Column selection uses named allowlists with
  `dplyr::all_of()`, so a missing expected column errors rather than
  silently producing a narrower table.
- **`build_revision` is uniform across lineages.** Any narrowing of its
  input set reintroduces stale-release risk.
- **The ledger is durable before return.** `append_ledger()` flushes and
  fsyncs both the file and its containing directory before returning, via
  the external `sync` binary, since base R exposes no `fsync(2)` binding.
  A nonzero exit aborts rather than being discarded.
- **Ratings apply to cards and factions only, never to people.**
- **No LLM indexing of nrdb free text.** Any code path that would do so
  must assert `LLM_USE_POLICY` with `stopifnot()` rather than relying on a
  comment, so the constraint fails loudly if reached.
- **No LLM indexing of cardpool, implementation, or rules free text
  (precautionary).** Any code path that would do so must assert
  `LLM_USE_POLICY_PRECAUTIONARY` with `stopifnot()`. Unlike
  `LLM_USE_POLICY`, this is a conservative default, not a confirmed
  per-source requirement.
- **ABR-sourced views must carry attribution.** Any view rendering ABR
  data must render a backlink to
  [alwaysberunning.net](https://alwaysberunning.net), guarded by
  `require_abr_attribution()`.
- **Implementation-sourced views must carry the MIT notice.** Any view
  rendering mtgred/netrunner-derived data must render its MIT copyright
  and permission notice, guarded by
  `require_implementation_license_notice()`.
- **Cardpool-sourced views must carry a non-affiliation disclaimer.** Any
  view rendering cardpool data must render a disclaimer that it is not
  maintained, produced, endorsed, supported, or affiliated with Fantasy
  Flight Games and/or Wizards of the Coast, guarded by
  `require_cardpool_disclaimer()`.
- **Rules-sourced views must carry a non-affiliation disclaimer.** Any
  view rendering rules data must render a disclaimer that it is not
  associated with, produced by, or endorsed by Fantasy Flight Games, R.
  Talsorian Games, or Wizards of the Coast, guarded by
  `require_rules_disclaimer()`.
- **Release resolution captures once.** `resolve_release()` derives raw
  and processed paths from a single `fs::path_real()` call, so a promote
  landing mid-read cannot hand one consumer two different releases.
