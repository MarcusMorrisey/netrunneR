# inst/scripts/

Executable entry points shipped with the installed package.

## Files

| File | What | When to read |
| --- | --- | --- |
| `sync.R` | `Rscript` wrapper delegating to `netrunneR_cli()`; the only place `quit()` is called, mapping `netrunneR_lock_contention` to exit status 4 | Changing container invocation, exit statuses, or top-level condition handling |

## Run

```sh
Rscript inst/scripts/sync.R --lineage cardpool --mode backfill
```
