test_that("safe_render() returns a fallback instead of propagating an error", {
  result <- safe_render(function() stop("boom"))
  expect_s3_class(result, "shiny.tag")
})

test_that("safe_render() passes through a successful render untouched", {
  result <- safe_render(function() "ok")
  expect_equal(result, "ok")
})

test_that("alert_box() builds a Bootstrap alert of the requested type", {
  result <- alert_box("something happened", "danger")
  expect_s3_class(result, "shiny.tag")
  expect_match(as.character(result), "alert-danger")
  expect_match(as.character(result), "something happened")
})

test_that("alert_box() rejects an unlisted type", {
  expect_error(alert_box("x", "nonsense"))
})

test_that("click_sets_input() builds a namespaced Shiny.setInputValue() call", {
  fake_session <- list(ns = function(id) paste0("mod-", id))
  result <- click_sets_input(fake_session, "card_clicked", "01001")
  expect_match(result, "Shiny.setInputValue")
  expect_match(result, "mod-card_clicked", fixed = TRUE)
  expect_match(result, "01001", fixed = TRUE)
})

test_that("select_tournaments_source() prefers a non-NULL merged read over an available abr release", {
  merged_release <- list(tables = list(tournament_merged = data.frame(id = "abr:1+cobra:1")))
  abr_result <- list(data = data.frame(id = "1"))
  result <- select_tournaments_source(merged_release, abr_result)
  expect_identical(result$id, "abr:1+cobra:1")
})

test_that("select_tournaments_source() falls back to abr when the merged read is entirely NULL", {
  abr_result <- list(data = data.frame(id = "1"))
  result <- select_tournaments_source(NULL, abr_result)
  expect_identical(result$id, "1")
})

test_that("select_tournaments_source() falls back to abr when cobra has no tournament_merged table", {
  merged_release <- list(tables = list(tournament_merged = NULL))
  abr_result <- list(data = data.frame(id = "1"))
  result <- select_tournaments_source(merged_release, abr_result)
  expect_identical(result$id, "1")
})

test_that("select_tournaments_source() returns NULL when both sources are unavailable", {
  expect_null(select_tournaments_source(NULL, NULL))
})
