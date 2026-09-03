# netrunneR

Offline mirror and analysis toolkit for Netrunner card-game data. The
package maintains a versioned, local, SQLite-backed mirror of six
upstream sources, promotes each new build atomically, and exposes derived
ratings, ice/breaker matchup and tournament-meta views shared by a scheduled
sync container and a Shiny app.

## What it mirrors

| Lineage          | Source type   | Upstream                                     | Mirrored content                        |
| ---------------- | ------------- | -------------------------------------------- | --------------------------------------- |
| `nrdb`           | `api_poll`    | [NetrunnerDB public API](https://netrunnerdb.com)                        | Reviews and rulings                     |
| `abr`            | `api_poll`    | [AlwaysBeRunning API](https://alwaysberunning.net)                           | Tournaments, entries, videos, upcoming  |
| `cobra`          | `api_poll`    | [NSG's Cobra tournament platform](https://tournaments.nullsignal.games) | Tournaments, stages, rounds, pairings, standings, faction/identity counts |
| `cardpool`       | `git_mirror`  | [Null Signal Games card JSON repository](https://github.com/Null-Signal-Games/netrunner-cards-json)        | Cycles, factions, packs, cards          |
| `implementation` | `git_mirror`  | [The Jinteki implementation repository](https://github.com/mtgred/netrunner)         | Normalized ice/breaker trait rows       |
| `rules`          | `web_archive` | [The Comprehensive Rules hub](https://nullsignal.games/rules/comp-rules/)                   | Rules PDFs, content-addressed, versioned |

Only these six are wired up. `new_lineage()` is a public extension point
for registering others; `assets` is a named extension point with no code
behind it and is not required for the extension point to work.

## What the app shows

Three views, all reading the same active releases rather than a release of
their own:

| View | Reads | Shows |
| --- | --- | --- |
| Ice::Breaker | `cardpool`, `implementation` | Ice lanes with per-lane breakers and a stat strip for each pair |
| Meta Maps | `abr`, `cardpool` | A country choropleth of tournaments per million, with a venue-density point layer |
| Meta Stats | `abr`, `cardpool` | Faction share of wins as an interactive treemap and a percentage waffle |

The two meta views share one filter bar, owned by the app rather than by
either view: a date range with rotation shortcuts, an All / competitive /
casual lens over the tournament type with a sub-type picker under it, and a
clear-all. Draft-identity results are excluded unconditionally and are not a
setting -- see `draft_identity_codes()`.

Their spatial and plotting dependencies (`sf`, `tmap`, `leaflet`, `ggplot2`,
`treemap`, `d3treeR`) are **Suggests**, not Imports. In Imports the package
would fail to load anywhere they are absent, which would couple the mirror to
a container rebuild for views most sessions never open. Every entry point
checks and degrades instead.

## Architecture

Each lineage is an S3 object tagged with its `source_type`, resolved from
a static registry that holds its schedule, schema version, pacing, build
module path and any per-type extras. Fetching and building dispatch on
that class; everything else is shared.

The shared sync pipeline runs the same eight steps for every lineage:
acquire a non-blocking per-lineage lock, fetch into a fresh staging
directory, short-circuit if content and build revision both match the
active release, build the SQLite output, validate, stage the manifest,
promote, and append a durable ledger record.

Promotion is atomic. A staged directory is moved into
`releases/<release_id>` and the `active` symlink is swapped, both as
same-filesystem renames, so `active` is never absent and never points at a
partial release. Rollback re-points `active` at an earlier release
directory using the same mechanism.

A release is identified by a composite of the upstream revision and a
build revision digested over the build code and DDL. Unchanged upstream
plus unchanged build code produces no new release; unchanged upstream plus
changed build code produces a distinct one. Both are needed, because a
schema or build fix must be able to produce a new release without waiting
for upstream to move.

Every release carries a manifest with its validation report embedded, and
every promotion, no-change, validation failure and rollback is appended to
a per-lineage ND-JSON ledger that is fsynced before the call returns.

## Design decisions

**Store roots are container-side paths.** Each lineage's store root is
`/data/<name>`, identical on both sides of the Docker bind mount. R code
never names a host path, so the package runs unchanged inside the sync
container or directly on a host filesystem, and host-side storage
reorganization requires no code change. A test asserts no `/srv` literal
exists in the source.

**The store must be a bind mount, not a named Docker volume.** Atomic
promotion is a `rename(2)` within one filesystem; a named volume's storage
driver does not guarantee that, and atomicity is the whole safety
property.

**One byte-capture boundary.** `capture_response_body()` is the only
function permitted to write HTTP response bytes to disk, so headers and
cookies fall out of scope immediately. A static scan and a
sentinel-header test enforce it.

**Personal data is excluded by two independent fail-closed layers.**
Processed tables are built through named column allowlists, and a separate
regex scan rejects personal-data-shaped column names before any write.
Ratings are computed for cards and factions only, never for people.

**ABR attribution is enforced in code.** Any view rendering ABR-sourced
data must render a backlink to [alwaysberunning.net](https://alwaysberunning.net),
guarded by `require_abr_attribution()` rather than by documentation alone --
and per that same requirement, this page itself credits every upstream
source in the table above.

**Upstream breakage is tombstoned, not papered over.** A persistently
failing upstream record is retried on a schedule and then excluded
outright rather than written as a placeholder row, so downstream consumers
never see partial data indistinguishable from real data.

`R/README.md` records the per-module rationale: the lineage dispatch
model, the ABR backfill and outage-detection design, the NetrunnerDB
envelope redesign, the rules-hub scraper structure, the removal of
decklist mirroring, and the full invariant list.

## Running it

Run a sync for one lineage:

```sh
Rscript -e 'netrunneR::netrunneR_cli(c("--lineage", "cardpool", "--mode", "backfill"))'
```

Or through the packaged CLI wrapper, which is what the container invokes:

```sh
Rscript inst/scripts/sync.R --lineage cardpool --mode backfill
```

Modes are `scheduled` (short-circuits when content is unchanged),
`backfill` (never short-circuits) and `rollback` (requires
`--release-id`). Lock contention exits with status 4 and writes no ledger
record: a concurrent run is a skip, not a failure.

**Zero-argument invocation (LINEAGE/MODE env vars).** When both
`--lineage` and `--mode` are omitted, the CLI falls back to the `LINEAGE`
and `MODE` environment variables instead of erroring immediately. This is
the invocation form the production deployment actually uses: the Compose
service and its systemd timer set `LINEAGE`/`MODE` in the environment and
run the wrapper with no flags at all. The two are required together in
that case -- if either is unset the CLI aborts with "nothing to run"
rather than guessing; a partial mix of one flag and one env var is not a
supported combination, since the fallback only triggers when *both* flags
are absent. `LINEAGE` accepts the same six lineage names as `--lineage`
(see the table above); `MODE` accepts the same three modes as `--mode`.

```sh
LINEAGE=cardpool MODE=backfill Rscript inst/scripts/sync.R
```

Serve the packaged Shiny app against the current release:

```sh
Rscript -e 'netrunneR::run_app()'
```

## Where the working tree lives

The canonical checkout is `/home/marcus/src/netrunneR`, a sibling of
`/home/marcus/src/homelab`. Three services bind-mount that exact path, so a
checkout anywhere else is invisible to all of them: the `netrunner-pkgdown`
container serves `docs/` from it, `netrunner-pkgdown-build.service` builds
pkgdown into that `docs/`, and rstudio-server mounts the tree at
`/projects/netrunneR`.

This is not hypothetical. A second clone at `/home/marcus/netrunneR` once
absorbed 28 commits of work while the canonical path sat at an older HEAD: the
published package site served a stale reference and the RStudio project opened
the wrong tree. Recovering needed a hard reset of the canonical checkout. Check
for the path before cloning; do not make a second copy.

The lineage stores are a separate matter and deliberately live outside both
repositories, at `/srv/netrunner-mirror/data/<lineage>` on the host and
`/data/<lineage>` inside the container. A release is data, not source, and
putting it in the tree would make every sync a working-tree change.

## Development

Checks, tests, documentation and coverage all run inside a
`rocker/r-ver:4.3.2` container against the bind-mounted working tree, so
they exercise the same installed-package code paths production does:

```sh
make check      # R CMD check, error_on = "warning"
make test       # devtools::test()
make document   # regenerate man/ and NAMESPACE from roxygen comments
make coverage   # covr::package_coverage()
```

`NRDB_CONTACT` must be set to a real operator contact address for
`check_config()` to pass outside the test suite; it is used to build the
outbound User-Agent, as NetrunnerDB requests. `check_config()` rejects an
empty string and obvious placeholders (`changeme`, `you@example.com`,
`TODO`), not just a missing value.

## License

MIT. See `LICENSE`.
