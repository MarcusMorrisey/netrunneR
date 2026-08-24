# tests/testthat/

Test files and shared fixtures. Several files enforce package invariants by static source scan rather than by behavior.

## Files

| File | What | When to read |
| --- | --- | --- |
| `setup.R` | Sets a non-placeholder `NRDB_CONTACT` for the whole suite | Adding a shared fixture or required env var for tests |
| `test-lineage.R` | Registry `store_root` resolution, S3 class tagging, staging containment, and the static scan asserting no `/srv` literal in `R/` | Adding a lineage, changing `store_root`, debugging the host-path scan |
| `test-capture-boundary.R` | Static AST scan plus Set-Cookie sentinel run enforcing the single byte-capture boundary | Adding a fetch path that touches response bytes |
| `test-sync.R` | `run_sync()` mode dispatch and the scheduled/backfill no-op short-circuit | Changing pipeline modes or no-op logic |
| `test-promote.R` | `promote()` move-then-swap, atomic `swap_active()`, `rollback()` guards | Changing promotion or rollback |
| `test-release.R` | `resolve_release()` single-capture path derivation and missing-active abort | Changing release resolution |
| `test-ledger.R` | Ledger append/read round-trip and `check_ledger_consistency()` | Adding ledger event types or changing durability |
| `test-lock-contention.R` | Lock contention surfacing as a typed condition, not process termination | Changing locking or contention handling |
| `test-build-revision.R` | Build revision identical across all five lineages; loud abort on missing inputs | Changing what feeds `build_revision()` |
| `test-validate.R` | `validate_release()` pass/fail aggregation and two check primitives | Adding a validation rule or changing aggregation |
| `test-cli.R` | Flag/env-var precedence, mode validation, `--release-id` gating | Adding a CLI flag or changing precedence |
| `test-fetch-api-poll.R` | Shared api_poll dispatch, pagination derivation, 5xx hard stop, checkpoint resume | Changing shared api-poll fetch behavior |
| `test-fetch-nrdb.R` | nrdb User-Agent construction, `compare_shape()`, bounded retries | Changing the nrdb fetch helper |
| `test-fetch-git-mirror.R` | Clone and checkout against a throwaway local repo, default `main` ref | Changing git-mirror fetch or ref defaults |
| `test-fetch-web-archive.R` | Hub index parsing against a fixture modeled on the real page, PDF pooling | Fixing the rules-hub scraper |
| `test-abr-backfill.R` | Checkpoint resume, tombstoning, and outage detection separating fresh failures from known-bad retries | Changing backfill, tombstone, or outage-abort logic |
| `test-abr-allowlist.R` | Fail-closed allowlist, deny-pattern scan, and tournament cardinality with permanent exclusions | Changing ABR columns or the personal-data defenses |
| `test-build-cardpool.R` | git_mirror dispatch by name and the cardpool build against fixture JSON | Changing cardpool build or git-mirror dispatch |
| `test-build-nrdb.R` | Review/ruling build against real field shapes, nested list-column dropping | Changing nrdb build or allowlists |
| `test-build-implementation.R` | Trait extraction and release_id shape against a fixture card-definition file | Changing trait extraction |
| `test-build-rules.R` | Rules table writing, version-monotonic warning behavior, PDF hash checks | Changing rules build or version-order policy |
| `test-views.R` | Elo identity/faction ratings and deterministic canonical game ordering | Changing rating parameters or tie-break ordering |

## Test

```sh
make test
```
