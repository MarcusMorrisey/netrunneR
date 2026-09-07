test_that("packaged cobra tournament date corrections file loads with the declared column types", {
  corrections <- read_cobra_tournament_date_corrections()
  expect_s3_class(corrections, "data.frame")
  expect_true(all(c("tournament_id", "corrected_date", "reason", "verified_by", "verified_at") %in% names(corrections)))
  expect_true(is.character(corrections$tournament_id))
  expect_true(is.character(corrections$corrected_date))
  expect_gt(nrow(corrections), 0)
  expect_true("4910" %in% corrections$tournament_id)
})

test_that("apply_cobra_tournament_date_corrections corrects a matched tournament_id and leaves others untouched", {
  tournaments <- data.frame(
    tournament_id = c("4910", "1234"),
    date = c("1900-07-13", "2026-06-01"),
    stringsAsFactors = FALSE
  )
  corrections <- data.frame(
    tournament_id = "4910", corrected_date = "2026-07-07",
    reason = "x", verified_by = "x", verified_at = "x",
    stringsAsFactors = FALSE
  )
  corrected <- apply_cobra_tournament_date_corrections(tournaments, corrections)
  expect_equal(corrected$date, c("2026-07-07", "2026-06-01"))
})

test_that("apply_cobra_tournament_date_corrections is a no-op when no tournament_id matches", {
  tournaments <- data.frame(
    tournament_id = "1234", date = "2026-06-01",
    stringsAsFactors = FALSE
  )
  corrections <- data.frame(
    tournament_id = "4910", corrected_date = "2026-07-07",
    reason = "x", verified_by = "x", verified_at = "x",
    stringsAsFactors = FALSE
  )
  corrected <- apply_cobra_tournament_date_corrections(tournaments, corrections)
  expect_equal(corrected$date, "2026-06-01")
})
