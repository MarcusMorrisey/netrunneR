test_that("safe_render() returns a fallback instead of propagating an error", {
  result <- safe_render(function() stop("boom"))
  expect_s3_class(result, "shiny.tag")
})

test_that("safe_render() passes through a successful render untouched", {
  result <- safe_render(function() "ok")
  expect_equal(result, "ok")
})
