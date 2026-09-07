# Covers the store-root seam (store_base() in R/lineage.R) and the
# temporary promoted store it makes possible
# (local_store_fixture(), tests/testthat/helper-store-fixture.R),
# including the app-data load that store exists to exercise.

test_that("store_base() defaults to the bind-mounted container path", {
  # DL-009: unset must stay byte-identical to what compose.yaml mounts,
  # or a deployment silently reads a different directory than it writes.
  withr::local_envvar(c(NETRUNNER_STORE_BASE = NA))
  expect_identical(store_base(), "/data")
  expect_identical(lineage("cardpool")$store_root, file.path("/data", "cardpool"))
})

test_that("store_base() honours the override and every lineage follows it", {
  withr::local_envvar(c(NETRUNNER_STORE_BASE = "/tmp/store"))
  expect_identical(store_base(), "/tmp/store")
  roots <- vapply(BUILTIN_LINEAGES, function(n) lineage(n)$store_root, character(1))
  expect_identical(unname(roots), file.path("/tmp/store", BUILTIN_LINEAGES))
})

test_that("store_base() treats an empty override as unset and trims a trailing slash", {
  # An empty env var is how a shell exports "nothing"; it must mean the
  # default, not a relative path rooted at "".
  withr::local_envvar(c(NETRUNNER_STORE_BASE = ""))
  expect_identical(store_base(), "/data")

  withr::local_envvar(c(NETRUNNER_STORE_BASE = "/tmp/store/"))
  expect_identical(store_base(), "/tmp/store")
  expect_identical(lineage("cardpool")$store_root, "/tmp/store/cardpool")
})

test_that("local_store_fixture() promotes releases resolve_release() can find", {
  base <- local_store_fixture()
  for (name in c("cardpool", "implementation")) {
    resolved <- resolve_release(lineage(name))
    expect_true(fs::dir_exists(resolved$processed_dir))
    expect_equal(basename(resolved$release_dir), "fixture-release")
    # active is a symlink into releases/, not a copy: that indirection is
    # what makes promote() atomic, so a fixture that copied instead would
    # not be testing the real layout.
    expect_true(fs::dir_exists(file.path(base, name, "releases", "fixture-release")))
  }
})

test_that("a lineage left out of the fixture has no active release", {
  local_store_fixture("cardpool")
  expect_type(resolve_release(lineage("cardpool")), "list")
  expect_error(resolve_release(lineage("implementation")), class = "netrunneR_no_active_release")
})

test_that("load_ice_breaker_app_data() loads from a promoted fixture store", {
  local_store_fixture()
  app_data <- load_ice_breaker_app_data()
  expect_null(app_data$missing_lineages)
  # `cards` stays restricted to ice_breaker_pool(): agn01 (an agenda) is
  # in the mini-pool cardpool but must not be in `cards`. See DL-035.
  ice_breaker_codes <- mini_pool_cardpool()$code[mini_pool_cardpool()$type_code %in% c("ice", "program")]
  expect_equal(sort(app_data$cards$code), sort(ice_breaker_codes))
  expect_true(nrow(app_data$matchup) > 0)
})

test_that("load_ice_breaker_app_data() exposes all_codes for every cardpool card", {
  local_store_fixture()
  app_data <- load_ice_breaker_app_data()
  expect_named(app_data$all_codes, c("code", "title", "type_code"))
  expect_equal(sort(app_data$all_codes$code), sort(mini_pool_cardpool()$code))

  # A known agenda code is absent from `cards` (ice_breaker_pool()) but
  # present in all_codes -- the distinction Deck Compare's unresolved
  # state depends on (DL-035).
  expect_true("agn01" %in% app_data$all_codes$code)
  expect_false("agn01" %in% app_data$cards$code)

  expect_true(nrow(app_data$all_codes) > nrow(app_data$cards))
})

test_that("load_ice_breaker_app_data() names exactly the lineages that are missing", {
  local_store_fixture("cardpool")
  expect_equal(load_ice_breaker_app_data()$missing_lineages, "implementation")
})

test_that("load_ice_breaker_app_data() reports both when nothing is promoted", {
  local_store_fixture(character(0))
  app_data <- load_ice_breaker_app_data()
  expect_equal(app_data$missing_lineages, c("cardpool", "implementation"))
  expect_null(app_data$cards)
})

test_that("a promoted release with no processed database still counts as missing", {
  # resolve_release() succeeds (active exists) but the db file does not,
  # which query_active_release() must treat as missing rather than
  # letting dbConnect() create an empty database on the spot.
  base <- local_store_fixture()
  fs::file_delete(file.path(base, "implementation", "releases", "fixture-release",
                            "processed", "implementation.sqlite"))
  expect_equal(load_ice_breaker_app_data()$missing_lineages, "implementation")
})

test_that("a release with no legality tables degrades instead of failing", {
  # The fixture store writes only the `card` table, which is exactly the
  # shape of a release promoted before the format/card-pool schema
  # landed. Every legality table must come back NULL, and the app data
  # must still load -- read_active_release_tables() treats an absent
  # table as NULL rather than letting dbReadTable() error.
  local_store_fixture()
  app_data <- load_ice_breaker_app_data()

  expect_null(app_data$missing_lineages)
  expect_named(app_data$legality, CARDPOOL_LEGALITY_TABLES)
  expect_true(all(vapply(app_data$legality, is.null, logical(1))))
})

test_that("read_active_release_tables() returns the tables a release does have", {
  local_store_fixture()
  out <- read_active_release_tables("cardpool", "cardpool.sqlite", c("card", "format"))
  expect_equal(sort(out$tables$card$code), sort(mini_pool_cardpool()$code))
  expect_null(out$tables$format)
})

# --- M-003: the merged abr+cobra tournament read and its abr-only fallback ---

test_that("with both abr and cobra active, the app frame carries merged rows with the same column names as before", {
  local_store_fixture(c("cardpool", "implementation", "abr", "cobra"))
  app_data <- load_ice_breaker_app_data()

  expect_null(app_data$missing_lineages)
  expect_identical(app_data$tournaments$id, "abr:9001")
  # Same column set the abr-only read always returned -- no consumer
  # needs to change for the merged read to serve it.
  expect_identical(
    sort(names(app_data$tournaments)),
    sort(c(
      "id", "title", "date", "format", "type", "location_state", "location_country",
      "location_lat", "location_lng", "players_count", "top_count",
      "winner_runner_identity", "winner_corp_identity"
    ))
  )
})

test_that("with cobra absent, the app frame equals the abr-only read", {
  local_store_fixture(c("cardpool", "implementation", "abr"))
  app_data <- load_ice_breaker_app_data()

  expect_null(app_data$missing_lineages)
  expect_identical(app_data$tournaments$id, "9001") # abr's own id, not the "abr:9001" merged key
  expect_identical(app_data$tournaments$title, "Fixture Store Championship")
})

test_that("with a cobra release predating tournament_merged, the app frame equals the abr-only read", {
  # Staged inline rather than as a second STORE_FIXTURE_TABLES() entry:
  # this is deliberately an OLDER/incomplete cobra shape (schema_version
  # 1, before M-001), not a second lineage the fixture registry otherwise
  # models one-per-name.
  base <- local_store_fixture(c("cardpool", "implementation", "abr"))
  cobra_root <- file.path(base, "cobra")
  staging_dir <- file.path(cobra_root, "staging", "pre-merge")
  processed_dir <- file.path(staging_dir, "processed")
  fs::dir_create(processed_dir)
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(processed_dir, "cobra.sqlite"))
  DBI::dbWriteTable(con, "tournament", data.frame(tournament_id = "1", name = "Old Cobra Event", stringsAsFactors = FALSE))
  DBI::dbDisconnect(con)
  promote(cobra_root, staging_dir, "pre-merge-release")

  app_data <- load_ice_breaker_app_data()

  expect_null(app_data$missing_lineages)
  expect_identical(app_data$tournaments$id, "9001") # falls back to abr, not an error over the missing table
})

test_that("with both abr and cobra absent, tournaments is NULL and startup still succeeds", {
  local_store_fixture(c("cardpool", "implementation"))
  app_data <- load_ice_breaker_app_data()

  expect_null(app_data$missing_lineages)
  expect_null(app_data$tournaments)
})
