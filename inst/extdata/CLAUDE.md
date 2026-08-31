# inst/extdata/

Hand-curated data files shipped inside the package and read via `system.file()`.

## Files

| File | What | When to read |
| --- | --- | --- |
| `matchup_overrides.csv` | Hand-verified (ice, breaker) break costs with provenance columns: `reason`, `verified_by`, `verified_at`. Ships as a header-only template | Recording a corrected break cost, or debugging why an override did not take effect |
