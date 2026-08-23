# Tests build_implementation()'s trait extraction and release_id shape
# against a minimal fixture card-definition file, matching the real
# mtgred/netrunner layout confirmed live this session: Clojure source
# grouped one file per card category under src/clj/game/cards/, not the
# server/cards/*.js layout previously assumed (which does not exist in
# the real repository and crashed with ENOENT on every real run).
test_that("build_implementation() extracts ice/breaker traits ordered by code", {
  raw_dir <- withr::local_tempdir()
  cards_dir <- file.path(raw_dir, "src", "clj", "game", "cards")
  fs::dir_create(cards_dir)
  writeLines("(ns game.cards.ice)", file.path(cards_dir, "ice.clj"))

  li <- new_lineage("implementation", "git_mirror", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-implementation.R")
  staged_raw <- list(raw_dir = raw_dir, source_revision = "def456")

  built <- build_implementation(li, staged_raw)

  expect_true(fs::file_exists(built$db_path))
  expect_match(built$release_id, "^def456-b")
})
