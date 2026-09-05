test_that("packaged abr/cobra override files load with the declared column types", {
  matches <- read_abr_cobra_verified_matches()
  expect_s3_class(matches, "data.frame")
  expect_true(all(c("abr_id", "cobra_tournament_id", "reason", "verified_by", "verified_at") %in% names(matches)))
  expect_true(is.character(matches$abr_id))
  expect_true(is.character(matches$cobra_tournament_id))
  expect_gt(nrow(matches), 0)

  deletes <- read_abr_cobra_verified_deletes()
  expect_s3_class(deletes, "data.frame")
  expect_true(all(c("source", "source_id", "reason", "verified_by", "verified_at") %in% names(deletes)))
  expect_gt(nrow(deletes), 0)
})

test_that("packaged override files satisfy their own invariants", {
  matches <- read_abr_cobra_verified_matches()
  deletes <- read_abr_cobra_verified_deletes()
  expect_true(validate_abr_cobra_overrides(matches, deletes))

  expect_equal(anyDuplicated(matches$abr_id), 0L)
  expect_equal(anyDuplicated(matches$cobra_tournament_id), 0L)
  expect_true(all(deletes$source == "cobra"))
  expect_length(intersect(matches$cobra_tournament_id, deletes$source_id[deletes$source == "cobra"]), 0)
})

test_that("validate_abr_cobra_overrides fails closed on a duplicate abr_id", {
  matches <- data.frame(
    abr_id = c("1", "1"), cobra_tournament_id = c("10", "11"),
    reason = "x", verified_by = "x", verified_at = "x",
    stringsAsFactors = FALSE
  )
  deletes <- data.frame(
    source = character(0), source_id = character(0),
    reason = character(0), verified_by = character(0), verified_at = character(0),
    stringsAsFactors = FALSE
  )
  expect_error(validate_abr_cobra_overrides(matches, deletes), "abr_id")
})

test_that("validate_abr_cobra_overrides fails closed on a duplicate cobra_tournament_id", {
  matches <- data.frame(
    abr_id = c("1", "2"), cobra_tournament_id = c("10", "10"),
    reason = "x", verified_by = "x", verified_at = "x",
    stringsAsFactors = FALSE
  )
  deletes <- data.frame(
    source = character(0), source_id = character(0),
    reason = character(0), verified_by = character(0), verified_at = character(0),
    stringsAsFactors = FALSE
  )
  expect_error(validate_abr_cobra_overrides(matches, deletes), "cobra_tournament_id")
})

test_that("validate_abr_cobra_overrides fails closed on a non-cobra delete row", {
  matches <- data.frame(
    abr_id = character(0), cobra_tournament_id = character(0),
    reason = character(0), verified_by = character(0), verified_at = character(0),
    stringsAsFactors = FALSE
  )
  deletes <- data.frame(
    source = "abr", source_id = "1",
    reason = "x", verified_by = "x", verified_at = "x",
    stringsAsFactors = FALSE
  )
  expect_error(validate_abr_cobra_overrides(matches, deletes), "only source == 'cobra'")
})

test_that("validate_abr_cobra_overrides fails closed when a cobra id is both matched and deleted", {
  matches <- data.frame(
    abr_id = "1", cobra_tournament_id = "10",
    reason = "x", verified_by = "x", verified_at = "x",
    stringsAsFactors = FALSE
  )
  deletes <- data.frame(
    source = "cobra", source_id = "10",
    reason = "x", verified_by = "x", verified_at = "x",
    stringsAsFactors = FALSE
  )
  expect_error(validate_abr_cobra_overrides(matches, deletes), "both verified matches and verified deletes")
})
