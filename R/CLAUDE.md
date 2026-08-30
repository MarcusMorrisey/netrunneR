# R/

Package source: five lineage mirrors sharing one fetch/build/validate/promote pipeline, dispatched by S3 on `source_type`.

## Files

| File | What | When to read |
| --- | --- | --- |
| `README.md` | Architecture, design decisions, invariants | Before changing pipeline shape, dispatch, or any fail-closed defense |
| `netrunneR-package.R` | Package doc block, `fetch_lineage()`/`build_lineage()` S3 generics, imports | Adding a generic, changing dispatch contract, adding an `@importFrom` |
| `lineage.R` | `.LINEAGE_REGISTRY`, `new_lineage()`, `lineage()`, `BUILTIN_LINEAGES`, `store_base()`/`STORE_BASE_ENV` (the `NETRUNNER_STORE_BASE` override; `/data` unless set, which only tests and local development should do) | Adding a lineage, changing `store_root`/`repo_url`/`hub_url`/schedule config, pointing a test at a temporary store |
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
| `operations.R` | `alert_box()`, `safe_render()`, `click_sets_input()`, `resolve_active_release()`/`query_active_release()` (also used by `build-implementation.R`'s `cardpool_codes()`), `read_matchup_overrides()`, `read_active_release_tables()`/`CARDPOOL_LEGALITY_TABLES`, `load_ice_breaker_app_data()` -- shared Shiny-app plumbing: styled alerts, render-error fallback, click-to-input wiring, and the once-per-process cardpool/implementation/matchup load `inst/shiny-app/app.R` calls | Adding a new primary render that should degrade gracefully, a new clickable element, or changing how the app loads its data |
| `mod_card_browser.R` | `mod_card_browser_ui()`/`mod_card_browser_server()` -- pool browsing with filters, a NetrunnerDB-syntax search box, a format selector, and the image grid; `browser_search_fields()` (the card + legality registries merged), `card_grid_tags()` | Changing card-browser filters, the search box, the format selector, or the click-to-detail wiring |
| `mod_matchup_explorer.R` | `mod_matchup_explorer_server()` (modal-only, so there is no `_ui()`), `matchup_modal_body()`, `empty_matchup_reason()` -- one card's matchups over `compute_ice_breaker_matchups()`'s output, opened from the card detail modal, format-filtered at display | Changing the matchup table, its format filter, its empty states, or its click-to-detail wiring |
| `mod_lane_board.R` | `mod_lane_board_ui()`/`mod_lane_board_server()`, `lane_ui()`, `matchup_pair_state()`, `assumed_pair_cost()`, `stat_strip_ui()`, `override_control_ui()` -- the landing screen: ice lanes with per-lane breakers and a stat strip whose five states are formula / override / assumed / cannot break / not computable | Changing the board, what a stat strip claims, or the per-lane subtype override |
| `views-meta.R` | `tournament_country_counts()`, `country_map_name()`, `tournament_venues()`, `parse_abr_date()`, `rotation_periods()` -- the abr shaping behind the meta views | Changing what the map counts, how a country name is matched, or how an abr date is read |
| `views-meta-stats.R` | `FACTION_COLOURS`/`FACTION_ORDER`, `tournament_faction_wins()` (side-filtered, reports `undecided` and `misfiled`), `faction_waffle_squares()`, `largest_remainder()`, `factions_below_resolution()`, `build_faction_waffle()`, `faction_treemap_hierarchy()` | Changing what the meta stats charts count, the faction palette or order, or the waffle's box arithmetic |
| `mod_date_filter.R` | `mod_date_filter_ui()`/`mod_date_filter_server()`, `with_parsed_dates()`, `date_bounds()`, `preset_range()`, `filter_by_date()` -- the date slider and rotation shortcuts, shared by BOTH meta views so "the same filter" is one implementation rather than two that drift | Changing the date filter anywhere; there is only one |
| `mod_meta_map.R` | `mod_meta_map_ui()`/`mod_meta_map_server()`, `world_polygons()`, `build_tournament_map()` -- the tournament choropleth and venue points; sf/tmap/leaflet are Suggests and every entry point degrades | Changing the map, its scale, or its layers |
| `mod_meta_stats.R` | `mod_meta_stats_ui()`/`mod_meta_stats_server()`, `no_release_box()` -- the meta statistics view: an interactive d3treeR treemap and a ggplot2 waffle over the shared date filter; ggplot2/treemap/d3treeR are Suggests and every entry point degrades | Changing the stats view, its charts, or what it says about data it left out |
| `mod_card_detail.R` | `mod_card_detail_ui()`/`mod_card_detail_server()` -- shared card-detail modal, instantiated once per session against a shared `selected_code` reactiveVal | Changing the detail modal or its selection-state contract |
| `app.R` | `run_app()`, `require_abr_attribution()`, `require_implementation_license_notice()`, `require_cardpool_disclaimer()`, `require_rules_disclaimer()`, `card_image_url()` | Wiring the Shiny app or adding an ABR-, implementation-, cardpool-, or rules-sourced view |
