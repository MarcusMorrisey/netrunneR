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
