# netrunneR

Offline mirror and analysis toolkit for Netrunner card-game data. The
package maintains a versioned, local, SQLite-backed mirror of five
upstream sources, promotes each new build atomically, and exposes derived
ratings and matchup views shared by a scheduled sync container and a Shiny
app.

## What it mirrors

| Lineage          | Source type   | Upstream                                     | Mirrored content                        |
| ---------------- | ------------- | -------------------------------------------- | --------------------------------------- |
| `nrdb`           | `api_poll`    | NetrunnerDB public API                        | Reviews and rulings                     |
| `abr`            | `api_poll`    | AlwaysBeRunning API                           | Tournaments, entries, videos, upcoming  |
| `cardpool`       | `git_mirror`  | Null Signal Games card JSON repository        | Cycles, factions, packs, cards          |
| `implementation` | `git_mirror`  | The Jinteki implementation repository         | Normalized ice/breaker trait rows       |
| `rules`          | `web_archive` | The Comprehensive Rules hub                   | Rules PDFs, content-addressed, versioned |

Only these five are wired up. `new_lineage()` is a public extension point
for registering others; `cobra` and `assets` are named extension points
with no code behind them and are not required for the extension point to
work.

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
data must render the `alwaysberunning.net` backlink, guarded by
`require_abr_attribution()` rather than by documentation alone.

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

Serve the packaged Shiny app against the current release:

```sh
Rscript -e 'netrunneR::run_app()'
```

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
outbound User-Agent, as NetrunnerDB requests.

## License

MIT. See `LICENSE`.
