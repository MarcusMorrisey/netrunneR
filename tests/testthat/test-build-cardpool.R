# Tests build_lineage.netrunneR_git_mirror()'s dispatch-by-name and the
# cardpool build path against fixture JSON.
test_that("build_lineage.netrunneR_git_mirror() dispatches to build_cardpool() for the cardpool lineage", {
  raw_dir <- withr::local_tempdir()
  fs::dir_create(file.path(raw_dir, "pack"))
  writeLines('[{"code":"c1","name":"core","position":1}]', file.path(raw_dir, "cycles.json"))
  writeLines('[{"code":"anarch","name":"Anarch","side":"runner"}]', file.path(raw_dir, "factions.json"))
  writeLines('[{"code":"p1","name":"Core Set","cycle_code":"c1","position":1}]', file.path(raw_dir, "packs.json"))

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
