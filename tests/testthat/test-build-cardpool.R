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

# ---- card_cycle -------------------------------------------------------

test_that("read_card_cycles() takes legacy_code from upstream, not from the id", {
  # The four oldest cycles are renames, not separator swaps. Deriving
  # legacy_code by replacing "_" with "-" would produce "core-set",
  # "revised-core-set", "napd-multiplayer" and "system-core-2019" -- none
  # of which is a cycle code -- so these four are the whole reason the
  # field is read rather than computed.
  path <- withr::local_tempfile(fileext = ".json")
  writeLines('[
    {"id":"core_set","legacy_code":"core","name":"Core Set","position":1,"released_by":"ffg"},
    {"id":"revised_core_set","legacy_code":"core2","name":"Revised Core","position":2,"released_by":"nsg"},
    {"id":"red_sand","legacy_code":"red-sand","name":"Red Sand","position":3,"released_by":"ffg"}
  ]', path)

  out <- read_card_cycles(path)

  expect_equal(nrow(out), 3L)
  expect_equal(out$legacy_code[out$id == "core_set"], "core")
  expect_equal(out$legacy_code[out$id == "revised_core_set"], "core2")
  expect_equal(out$legacy_code[out$id == "red_sand"], "red-sand")
  expect_false(any(grepl("_", out$legacy_code)))
})

test_that("read_card_cycles() returns the empty shape for an absent v2 tree", {
  # An older mirrored commit predates v2/card_cycles.json; the build
  # tolerates that rather than failing, exactly as it does for card_sets.
  path <- withr::local_tempfile(fileext = ".json")
  writeLines("[]", path)
  out <- read_card_cycles(path)
  expect_equal(nrow(out), 0L)
  expect_named(out, c("id", "legacy_code", "name", "position", "released_by"))
  expect_identical(names(out), names(empty_card_cycle()))
})

test_that("the cardpool schema gives both card_cycle_id columns a target", {
  # These two were the only cross-entity columns in the schema without a
  # REFERENCES clause, because the entity they named was not ingested.
  sql <- paste(
    readLines(system.file("sql/schema/cardpool.sql", package = "netrunneR"), warn = FALSE),
    collapse = "\n"
  )
  skip_if(!nzchar(sql), "schema not resolvable from installed package")

  expect_match(sql, "CREATE TABLE card_cycle")
  expect_match(sql, "legacy_code TEXT NOT NULL REFERENCES cycle[(]code[)]")
  # Both referencing columns, not just one.
  expect_equal(
    length(gregexpr("card_cycle_id TEXT NOT NULL REFERENCES card_cycle[(]id[)]", sql)[[1]]),
    2L
  )
})

test_that("card_cycle_ref_check() passes when every reference resolves", {
  cc <- tibble::tibble(id = c("core_set", "red_sand"), legacy_code = c("core", "red-sand"))
  sets <- tibble::tibble(card_cycle_id = c("core_set", "red_sand"))
  pool <- tibble::tibble(card_cycle_id = "core_set")
  cyc <- tibble::tibble(code = c("core", "red-sand"))

  out <- card_cycle_ref_check(cc, sets, pool, cyc)

  expect_equal(out$status, "pass")
  expect_match(out$message, "2 card cycles")
})

test_that("card_cycle_ref_check() fails on each of the three ways a reference can dangle", {
  # SQLite enforces none of these, so if this check does not catch them
  # nothing does -- the failure surfaces much later as an empty join.
  cc <- tibble::tibble(id = "core_set", legacy_code = "core")
  cyc <- tibble::tibble(code = "core")
  ok_sets <- tibble::tibble(card_cycle_id = "core_set")
  ok_pool <- tibble::tibble(card_cycle_id = "core_set")

  # (a) a card_set naming an unknown cycle
  a <- card_cycle_ref_check(cc, tibble::tibble(card_cycle_id = "ghost_cycle"), ok_pool, cyc)
  expect_equal(a$status, "fail")
  expect_match(a$message, "ghost_cycle")

  # (b) a card_pool_cycle naming an unknown cycle
  b <- card_cycle_ref_check(cc, ok_sets, tibble::tibble(card_cycle_id = "ghost_pool"), cyc)
  expect_equal(b$status, "fail")
  expect_match(b$message, "ghost_pool")

  # (c) a legacy_code that is not a v1 cycle -- the case a string-derived
  # mapping would have produced for core_set, revised_core_set,
  # napd_multiplayer and system_core_2019.
  c_ <- card_cycle_ref_check(
    tibble::tibble(id = "core_set", legacy_code = "core-set"), ok_sets, ok_pool, cyc
  )
  expect_equal(c_$status, "fail")
  expect_match(c_$message, "core-set")
})
