# inst/sql/

SQL assets applied at build time by `apply_schema()` in `R/build-cardpool.R`.

## Files

| File | What | When to read |
| --- | --- | --- |
| `README.md` | Why a DDL edit is never local to one lineage | Before editing any schema file |

## Subdirectories

| Directory | What | When to read |
| --- | --- | --- |
| `schema/` | One DDL file per lineage, applied fresh to each build's SQLite file | Changing a lineage's tables or columns, adding a lineage's schema |
