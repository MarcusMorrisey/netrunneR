# R/

Package source: five lineage mirrors sharing one fetch/build/validate/promote pipeline, dispatched by S3 on `source_type`.

## Files

| File | What | When to read |
| --- | --- | --- |
| `README.md` | Architecture, design decisions, invariants | Before changing pipeline shape, dispatch, or any fail-closed defense |
| `netrunneR-package.R` | Package doc block, `fetch_lineage()`/`build_lineage()` S3 generics, imports | Adding a generic, changing dispatch contract, adding an `@importFrom` |
| `lineage.R` | `.LINEAGE_REGISTRY`, `new_lineage()`, `lineage()`, `BUILTIN_LINEAGES` | Adding a lineage, changing `store_root`/`repo_url`/`hub_url`/schedule config |
| `sync.R` | `run_sync()` eight-step pipeline, `no_op_change()`, manifest write, `prune_releases()` | Changing pipeline order, no-op short-circuit logic, retention, sync logging |
| `promote.R` | `acquire_lock()`, `promote()`, `swap_active()`, `rollback()` | Debugging atomicity, lock contention, active-symlink swaps, rollbacks |
| `release.R` | `resolve_release()`, `release_entropy_suffix()` | Debugging release resolution races or release_id suffix collisions |
| `ledger.R` | ND-JSON append-only ledger, fsync-on-append, `check_ledger_consistency()` | Adding ledger event types, debugging lost or inconsistent ledger records |
| `build-revision.R` | Digest over build modules, shared modules, DDL, schema version, renv.lock | Changing what invalidates a build, debugging unexpected rebuilds or no-ops |
| `validate.R` | `validate_release()` check aggregation into the manifest report | Adding a validation stage, changing pass/warn/fail aggregation |
| `validate-helpers.R` | Check primitives: set membership, distinctness, row-count delta, deny pattern | Adding a validation rule, tuning row-drop thresholds, editing deny regexes |
| `capture.R` | `capture_response_body()`, the sole httr2-bytes-to-disk boundary | Adding any fetch path that touches response bytes |
| `config.R` | `LLM_USE_POLICY`, `LLM_USE_POLICY_PRECAUTIONARY`, `check_config()` startup validation | Adding a required env var or runtime policy constant |
| `cli.R` | `netrunneR_cli()`, `resolve_cli_triple()`, `NETRUNNER_MODES` | Adding a CLI flag, changing flag/env-var precedence or mode validation |
| `fetch-api-poll.R` | S3 fetch method for api_poll lineages, delegating to abr/nrdb helpers | Changing shared api-poll pacing, user-agent, or delegation |
| `fetch-abr.R` | ABR tournaments/entries/videos/upcoming fetch, throttling, 5xx hard stop | Debugging ABR fetches, pagination, cookie-jar handling, row-limit caps |
| `abr-backfill.R` | Resumable ABR entries backfill, checkpoints, tombstones, outage detection | Debugging stalled backfills, tombstoned tournaments, false outage aborts |
| `fetch-nrdb.R` | NetrunnerDB reviews/rulings fetch, `compare_shape()`, retry backoff | Debugging nrdb fetches or the response-envelope shape check |
| `fetch-git-mirror.R` | S3 fetch method cloning and checking out a configured ref with gert | Changing clone behavior, ref defaults, or git-derived `source_revision` |
| `fetch-web-archive.R` | Rules hub scrape, `parse_rules_hub_index()`, `head_last_modified()`, `pool_pdf()` | Fixing the hub scraper, PDF pooling, or version/date extraction |
| `build-abr.R` | api_poll build dispatch, `ABR_TOURNAMENT_ALLOWLIST`, `build_abr()` | Changing ABR columns, cardinality checks, or api_poll build dispatch |
| `build-nrdb.R` | `NRDB_REVIEW_ALLOWLIST`, `NRDB_RULING_ALLOWLIST`, `build_nrdb()` | Changing nrdb columns or review/ruling table construction |
| `build-cardpool.R` | git_mirror build dispatch, cardpool allowlists, `read_json_tibble()`, `apply_schema()` | Changing cardpool tables, JSON ingestion, or schema application |
| `build-implementation.R` | Ice/breaker trait extraction, cardpool code cross-check | Changing trait parsing or the non-blocking cardpool cross-check |
| `build-rules.R` | web_archive build dispatch, `check_version_monotonic()`, `check_pdf_hashes()` | Changing rules tables, version-order policy, or PDF hash re-verification |
| `views-ratings.R` | `compute_identity_ratings()`, `canonical_game_order()`, `run_elo()` | Changing rating parameters, tie-break ordering, or Elo inputs |
| `views-matchups.R` | `compute_ice_breaker_matchups()` (now takes required `matchup_overrides`, emits `source`/`credit_differential`), `compute_cost_to_break_formula()` stub, `build_view_manifest()` | Changing matchup expansion, subtype compatibility, override precedence, or view cache identity |
| `operations.R` | `alert_box()`, `safe_render()`, `click_sets_input()`, `resolve_active_release()`/`query_active_release()` (also used by `build-implementation.R`'s `cardpool_codes()`), `load_ice_breaker_app_data()` -- shared Shiny-app plumbing: styled alerts, render-error fallback, click-to-input wiring, and the once-per-process cardpool/implementation/matchup load `inst/shiny-app/app.R` calls | Adding a new primary render that should degrade gracefully, a new clickable element, or changing how the app loads its data |
| `mod_card_browser.R` | `mod_card_browser_ui()`/`mod_card_browser_server()` -- pool browsing with filters, image grid | Changing card-browser filters or the click-to-detail wiring |
| `mod_matchup_explorer.R` | `mod_matchup_explorer_ui()`/`mod_matchup_explorer_server()` -- ice-vs-breaker comparison over `compute_ice_breaker_matchups()`'s output | Changing the matchup table or its click-to-detail wiring |
| `mod_card_detail.R` | `mod_card_detail_ui()`/`mod_card_detail_server()` -- shared card-detail modal, instantiated once per session against a shared `selected_code` reactiveVal | Changing the detail modal or its selection-state contract |
| `app.R` | `run_app()`, `require_abr_attribution()`, `require_implementation_license_notice()`, `require_cardpool_disclaimer()`, `require_rules_disclaimer()`, `card_image_url()` | Wiring the Shiny app or adding an ABR-, implementation-, cardpool-, or rules-sourced view |
