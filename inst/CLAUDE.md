# inst/

Assets installed alongside the package and reachable at runtime via `system.file()`.

## Subdirectories

| Directory | What | When to read |
| --- | --- | --- |
| `scripts/` | `sync.R`, the container entry point wrapping `netrunneR_cli()` | Changing the container entry point or process exit-status handling |
| `sql/` | Per-lineage DDL applied fresh to each build's SQLite file | Changing a lineage's tables, columns, or constraints |
| `shiny-app/` | Shiny UI and server entry points served by `run_app()` | Implementing app views, wiring ABR attribution into the UI |
| `extdata/` | Hand-curated data shipped with the package, reachable via `system.file()` | Adding a curated override or other packaged data file |
| `pkg-src/` | Generated, git-ignored mirror of `R/` and `renv.lock`, recreated by the Dockerfile and by `make check` | Never edit directly and never commit; read only when debugging `build_revision()` path resolution |
