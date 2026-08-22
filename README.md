# netrunneR

Offline mirror and analysis toolkit for Netrunner card-game data. See
`docs/netrunneR/offline-mirror-plan.md` in the homelab repo for the full
design spec.

Run a sync locally with `netrunneR::netrunneR_cli(c("--lineage", "cardpool", "--mode", "backfill"))`,
or via the packaged CLI wrapper at `inst/scripts/sync.R`.

Only nrdb, abr, cardpool, rules and implementation are wired up; cobra and
assets are documented extension points with no code behind them yet.
