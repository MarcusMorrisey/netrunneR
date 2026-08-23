# Tests build_lineage.netrunneR_git_mirror()'s dispatch-by-name and the
# cardpool build path against fixture JSON.
#
# raw_dir is always nested one level under a fresh attempt directory
# (file.path(attempt_dir, "raw")), matching what
# fetch_lineage.netrunneR_git_mirror() actually produces in production
# (raw_dir <- file.path(attempt_dir, "raw")). build_cardpool() derives
# db_path from dirname(raw_dir), i.e. attempt_dir; a bare
# withr::local_tempdir() used directly as raw_dir has no such per-attempt
# parent, so its dirname() collapses to the single R-session-wide
# tempdir() shared by every test in this file, making two tests that both
# reach a successful build collide on the same db_path.
test_that("build_lineage.netrunneR_git_mirror() dispatches to build_cardpool() for the cardpool lineage", {
  raw_dir <- file.path(withr::local_tempdir(), "raw")
  fs::dir_create(file.path(raw_dir, "pack"))
  writeLines('[{"code":"c1","name":"core","position":1}]', file.path(raw_dir, "cycles.json"))
  writeLines('[{"code":"anarch","name":"Anarch","side":"runner"}]', file.path(raw_dir, "factions.json"))
  writeLines('[{"code":"p1","name":"Core Set","cycle_code":"c1","position":1}]', file.path(raw_dir, "packs.json"))
  writeLines(
    '[{"code":"01001","title":"Sure Gamble","pack_code":"p1","faction_code":"anarch","type_code":"event","side_code":"runner"}]',
    file.path(raw_dir, "pack", "p1.json")
  )

  li <- new_lineage("cardpool", "git_mirror", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-cardpool.R")
  staged_raw <- list(raw_dir = raw_dir, source_revision = "abc123")

  built <- build_lineage.netrunneR_git_mirror(li, staged_raw)

  expect_true(fs::file_exists(built$db_path))
  expect_match(built$release_id, "^abc123-b")
})

test_that("build_lineage.netrunneR_git_mirror() rejects an unrecognized git-mirror lineage name", {
  li <- new_lineage("unknown-lineage", "git_mirror", withr::local_tempdir())
  expect_error(
    build_lineage.netrunneR_git_mirror(li, list(raw_dir = withr::local_tempdir())),
    class = "netrunneR_no_build_method"
  )
})

test_that("build_cardpool() drops unrecognized upstream keys instead of erroring", {
  # Mirrors the real Null-Signal-Games/netrunner-cards-json shape, which
  # carries materially more fields per record (rotated/size/date_release/
  # ffg_id/flavor/illustrator/quantity/uniqueness/...) than
  # inst/sql/schema/cardpool.sql declares, and names the faction table's
  # side column side_code upstream (matching the card table's own
  # side_code) rather than the schema's side. raw_dir is nested under a
  # fresh attempt directory -- see the dirname(raw_dir)/db_path note on
  # the dispatch test above.
  raw_dir <- file.path(withr::local_tempdir(), "raw")
  fs::dir_create(file.path(raw_dir, "pack"))
  writeLines(
    '[{"code":"c1","name":"core","position":1,"rotated":true,"size":1}]',
    file.path(raw_dir, "cycles.json")
  )
  writeLines(
    '[{"code":"anarch","name":"Anarch","color":"FF4500","color_xterm":202,"is_mini":false,"side_code":"runner"}]',
    file.path(raw_dir, "factions.json")
  )
  writeLines(
    '[{"code":"p1","name":"Core Set","cycle_code":"c1","position":1,"date_release":"2012-09-06","ffg_id":1,"size":1}]',
    file.path(raw_dir, "packs.json")
  )
  writeLines(
    '[{"code":"01001","title":"Sure Gamble","pack_code":"p1","faction_code":"anarch","type_code":"event","side_code":"runner","quantity":3,"illustrator":"Someone","uniqueness":false}]',
    file.path(raw_dir, "pack", "p1.json")
  )

  li <- new_lineage("cardpool", "git_mirror", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-cardpool.R")
  staged_raw <- list(raw_dir = raw_dir, source_revision = "def456")

  built <- build_lineage.netrunneR_git_mirror(li, staged_raw)

  expect_true(fs::file_exists(built$db_path))
  unknown_check <- Filter(function(c) identical(c$check, "unknown_upstream_keys"), built$checks)[[1]]
  expect_identical(unknown_check$status, "warn")
  expect_match(unknown_check$message, "rotated")
  expect_match(unknown_check$message, "quantity")
  expect_no_match(unknown_check$message, "faction\\.side_code")
  expect_match(unknown_check$message, "faction\\.color")

  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  on.exit(DBI::dbDisconnect(con))
  expect_identical(
    sort(DBI::dbListFields(con, "card")),
    sort(CARDPOOL_CARD_ALLOWLIST)
  )
  expect_identical(DBI::dbGetQuery(con, "SELECT side FROM faction WHERE code = 'anarch'")$side, "runner")
})
