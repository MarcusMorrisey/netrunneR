# Covers append_ledger()/read_ledger() round-trip and check_ledger_consistency()'s disagreement guard.
test_that("append_ledger() writes an ND-JSON line read_ledger() parses back", {
  store_root <- withr::local_tempdir()
  append_ledger(store_root, list(event = "promoted", lineage = "cardpool", release_id = "r1"))

  records <- read_ledger(store_root)
  expect_length(records, 1)
  expect_identical(records[[1]]$event, "promoted")
})

test_that("check_ledger_consistency() aborts when ledger and active disagree", {
  store_root <- withr::local_tempdir()
  li <- new_lineage("cardpool", "git_mirror", store_root)

  append_ledger(store_root, list(event = "promoted", lineage = "cardpool", release_id = "r1"))
  target_dir <- file.path(store_root, "releases", "r2")
  fs::dir_create(target_dir)
  swap_active(store_root, target_dir)

  expect_error(check_ledger_consistency(li), class = "netrunneR_ledger_mismatch")
})

test_that("check_ledger_consistency() passes when no promoted record exists yet", {
  store_root <- withr::local_tempdir()
  li <- new_lineage("cardpool", "git_mirror", store_root)
  expect_true(check_ledger_consistency(li))
})
