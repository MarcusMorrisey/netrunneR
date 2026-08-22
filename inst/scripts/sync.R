#!/usr/bin/env Rscript
# Entry point invoked as `Rscript sync.R` inside the rocker container;
# delegates all argument handling to netrunneR_cli().
# Process exit is confined to this wrapper: no function under R/ calls
# quit() itself. run_sync() signals netrunneR_lock_contention instead of
# terminating the process, so this is the only place status 4 is chosen.
tryCatch(
  netrunneR::netrunneR_cli(commandArgs(trailingOnly = TRUE)),
  netrunneR_lock_contention = function(e) quit(save = "no", status = 4L)
)
