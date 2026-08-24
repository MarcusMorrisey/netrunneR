# inst/sql/

SQL assets applied at build time by `apply_schema()` in `R/build-cardpool.R`.

## Subdirectories

| Directory | What | When to read |
| --- | --- | --- |
| `schema/` | One DDL file per lineage, applied fresh to each build's SQLite file | Changing a lineage's tables or columns, adding a lineage's schema |

Every file under `schema/` is hashed into `build_revision()`, so any DDL edit forces a new release for all five lineages; see `R/build-revision.R`.
