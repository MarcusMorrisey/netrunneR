# Tests resolve_cli_triple()'s flag/env-var precedence, mode validation,
# and --release-id gating (see R/cli.R).
test_that("resolve_cli_triple() prefers explicit flags over env vars", {
  withr::local_envvar(LINEAGE = "nrdb", MODE = "backfill")
  opts <- list(lineage = "abr", mode = "scheduled", release_id = NULL)
  triple <- resolve_cli_triple(opts)
  expect_identical(triple$lineage, "abr")
  expect_identical(triple$mode, "scheduled")
})

test_that("resolve_cli_triple() falls back to LINEAGE/MODE only when both flags are absent", {
  withr::local_envvar(LINEAGE = "rules", MODE = "scheduled")
  triple <- resolve_cli_triple(list(lineage = NULL, mode = NULL, release_id = NULL))
  expect_identical(triple$lineage, "rules")
  expect_identical(triple$mode, "scheduled")
})

test_that("resolve_cli_triple() rejects a mode outside the documented set", {
  expect_error(
    resolve_cli_triple(list(lineage = "abr", mode = "daily", release_id = NULL)),
    class = "netrunneR_cli_parse_error"
  )
})

test_that("resolve_cli_triple() rejects --release-id outside --mode rollback", {
  expect_error(
    resolve_cli_triple(list(lineage = "abr", mode = "scheduled", release_id = "r1")),
    class = "netrunneR_cli_parse_error"
  )
})

test_that("resolve_cli_triple() errors when neither flags nor env vars are set", {
  withr::local_envvar(LINEAGE = "", MODE = "")
  expect_error(resolve_cli_triple(list(lineage = NULL, mode = NULL, release_id = NULL)),
               class = "netrunneR_cli_parse_error")
})
