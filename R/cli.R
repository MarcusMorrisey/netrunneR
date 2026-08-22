#' Parse CLI arguments and dispatch to run_sync() or rollback()
#'
#' Resolves the (lineage, mode, release_id) triple by taking explicit
#' --lineage/--mode/--release-id flags first, falling back to the LINEAGE
#' and MODE environment variables only when both flags are absent.
#' --release-id is a parse error outside --mode rollback. This env-var
#' fallback only ever fires when LINEAGE and MODE were already exported in
#' the invoking shell before `docker compose run` -- Compose interpolates
#' ${LINEAGE}/${MODE} in compose.yaml at parse time, before the container
#' or any --lineage/--mode flag exists, so the shell export is required
#' for every invocation form, not only the zero-argument scheduled one;
#' the resolved-store_root log line below exists in part so a rollout
#' check can catch a case where that export was forgotten.
#'
#' @param args Character vector of command-line arguments, typically
#'   `commandArgs(trailingOnly = TRUE)`.
#'
#' @return Invisibly, the result of run_sync().
#' @export
netrunneR_cli <- function(args) {
  parser <- optparse::OptionParser(option_list = list(
    optparse::make_option("--lineage", type = "character", default = NULL),
    optparse::make_option("--mode", type = "character", default = NULL),
    optparse::make_option("--release-id", type = "character", default = NULL, dest = "release_id")
  ))
  opts <- optparse::parse_args(parser, args = args)

  triple <- resolve_cli_triple(opts)

  check_config()

  li <- lineage(triple$lineage)
  cli::cli_inform("Resolved store_root for lineage {li$name}: {li$store_root}")

  run_sync(li, mode = triple$mode, release_id = triple$release_id)
}

#' The documented mode vocabulary run_sync() and the CLI recognize
#'
#' MODE is always one of these three execution modes, never a schedule
#' frequency: the systemd units' timer already encodes daily/monthly via
#' OnCalendar, so every timer-triggered invocation sets Environment=MODE=scheduled.
#' @keywords internal
NETRUNNER_MODES <- c("scheduled", "backfill", "rollback")

#' Resolve (lineage, mode, release_id) from parsed flags and env vars
#'
#' Explicit flags win outright; the LINEAGE/MODE environment variables are
#' consulted only when BOTH --lineage and --mode are absent, matching the
#' zero-argument scheduled invocation form Compose produces. Every
#' resolved mode, from either source, is validated against
#' NETRUNNER_MODES so a typo'd or stale mode value fails loudly instead of
#' silently reaching run_sync().
#'
#' @return A list with elements `lineage`, `mode`, and `release_id`.
#' @keywords internal
resolve_cli_triple <- function(opts) {
  if (!is.null(opts$release_id) && !identical(opts$mode, "rollback")) {
    rlang::abort("--release-id is only valid with --mode rollback", class = "netrunneR_cli_parse_error")
  }

  if (is.null(opts$lineage) && is.null(opts$mode)) {
    lineage_name <- Sys.getenv("LINEAGE", unset = "")
    mode <- Sys.getenv("MODE", unset = "")
    if (!nzchar(lineage_name) || !nzchar(mode)) {
      rlang::abort(
        "No --lineage/--mode flags and no LINEAGE/MODE environment variables; nothing to run.",
        class = "netrunneR_cli_parse_error"
      )
    }
    assert_known_mode(mode)
    return(list(lineage = lineage_name, mode = mode, release_id = NULL))
  }

  if (is.null(opts$lineage) || is.null(opts$mode)) {
    rlang::abort("--lineage and --mode must both be supplied when using explicit flags", class = "netrunneR_cli_parse_error")
  }

  assert_known_mode(opts$mode)
  list(lineage = opts$lineage, mode = opts$mode, release_id = opts$release_id)
}

#' Validate a mode string against NETRUNNER_MODES
#' @param mode Character scalar to validate.
#' @return TRUE, invisibly, if `mode` is valid; errors otherwise.
#' @keywords internal
assert_known_mode <- function(mode) {
  if (!mode %in% NETRUNNER_MODES) {
    rlang::abort(
      sprintf("Unknown mode '%s'. Must be one of: %s", mode, paste(NETRUNNER_MODES, collapse = ", ")),
      class = "netrunneR_cli_parse_error"
    )
  }
  invisible(TRUE)
}
