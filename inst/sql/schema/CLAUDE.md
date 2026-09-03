# inst/sql/schema/

One DDL file per lineage, applied fresh to each build's SQLite file before any data is written.

## Files

| File | What | When to read |
| --- | --- | --- |
| `abr.sql` | `tournament` table, columns exactly matching `ABR_TOURNAMENT_ALLOWLIST` | Adding an ABR column, changing the tournament table |
| `nrdb.sql` | `review` and `ruling` tables matching the real API response fields | Adding an nrdb column, changing review or ruling structure |
| `cobra.sql` | Ten tables (`tournament`, `stage`, `round`, `pairing`, `standing`, `faction_count`, `identity_count`, `cut_conversion_faction`, `cut_conversion_identity`, `recent_index`) matching `COBRA_*_ALLOWLIST`, no player display-name column | Adding a Cobra column or table |
| `cardpool.sql` | `cycle`, `faction`, `pack`, `card` tables and their relations | Adding a cardpool table or column, changing foreign keys |
| `implementation.sql` | `ice_breaker_traits`, keyed by the same `code` cardpool uses | Changing extracted trait columns |
| `rules.sql` | `rules_version`, carrying `pooled_hash` per PDF | Changing rules version columns or hash storage |

An edit to any file here changes `build_revision()` for every lineage and forces a rebuild; see `R/build-revision.R`.
