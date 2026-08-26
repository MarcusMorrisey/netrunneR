# netrunneR

R package maintaining an offline, versioned mirror of five Netrunner data sources with atomic release promotion, plus derived ratings and matchup views.

## Files

| File | What | When to read |
| --- | --- | --- |
| `README.md` | Package overview, architecture, design decisions, invariants | Orienting on the package, before changing the store or lineage model |
| `DESCRIPTION` | Package metadata, Imports/Suggests, testthat edition, roxygen version | Adding a dependency, bumping the version, changing package metadata |
| `NAMESPACE` | Generated exports and imports (roxygen output) | Never edit directly; regenerate via `make document`, then COMMIT the result -- `make docs-current` fails if you do not |
| `NEWS.md` | Release notes per version | Preparing a release, checking what shipped in a version |
| `Makefile` | Dockerized check/test/document/coverage targets and the apt library set | Running checks, adding a target, fixing missing system libraries |
| `renv.lock` | Pinned dependency versions restored by `.ci/restore.R` | Adding or upgrading a dependency, debugging a restore failure |
| `LICENSE` | MIT license text | Checking licensing terms |
| `.gitignore` | Ignored session artifacts, `renv/library`, `inst/pkg-src`, check output | Adding a generated path that must not be committed |
| `.Rbuildignore` | Paths excluded from the build tarball and `R CMD check` | Adding a dev-only directory that must not ship in the package |

## Subdirectories

| Directory | What | When to read |
| --- | --- | --- |
| `R/` | Package source: lineages, sync pipeline, fetch/build dispatch, views | Implementing or debugging any package behavior |
| `tests/` | testthat entry point and the full test suite | Adding tests, debugging failures, checking enforced invariants |
| `inst/` | Installed assets: CLI wrapper, Shiny app, SQL schemas | Changing the container entry point, the app, or a lineage's DDL |
| `.ci/` | Rscript entry points for check, test, document, coverage, renv restore | Changing what CI runs or how dependencies are restored |
| `man/` | Generated roxygen2 `.Rd` documentation (137 files) | Never edit directly; regenerate via `make document` after editing roxygen comments in `R/*.R`, then COMMIT the result -- `make docs-current` fails if you do not |

## Build

All targets run inside a `rocker/r-ver:4.3.2` container against the bind-mounted working tree.

```sh
make check      # R CMD check via rcmdcheck, error_on = "warning"
```

## Test

```sh
make test       # devtools::test()
make coverage   # covr::package_coverage()
```

## Regenerate documentation

```sh
make document      # roxygen2::roxygenise() -> man/*.Rd and NAMESPACE
make docs-current  # fails if man/ or NAMESPACE in git are out of date
```

`make document` writes into the working tree; the result must then be
COMMITTED. Forgetting that once shipped a master whose NAMESPACE was
missing five exports and whose man/ was missing 54 pages -- while every
`make check` and `make test` run stayed green, because `make document`
had already repaired the tree those commands went on to validate.
`make docs-current` is the guard: it compares against git HEAD, so it
catches regenerated-but-uncommitted output, which a check that merely
re-ran roxygen could not. Run it before pushing roxygen changes.

## Run a sync locally

```sh
Rscript -e 'netrunneR::netrunneR_cli(c("--lineage", "cardpool", "--mode", "backfill"))'
```
