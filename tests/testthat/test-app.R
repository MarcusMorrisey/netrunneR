# Tests the app.R guard functions that turn attribution/notice/disclaimer
# obligations into hard assertions rather than documentation conventions
# (see R/app.R).
test_that("require_abr_attribution() passes when has_attribution is TRUE", {
  expect_true(require_abr_attribution(TRUE))
})

test_that("require_abr_attribution() aborts when has_attribution is not TRUE", {
  expect_error(require_abr_attribution(FALSE))
  expect_error(require_abr_attribution(NA))
})

test_that("require_implementation_license_notice() passes when has_notice is TRUE", {
  expect_true(require_implementation_license_notice(TRUE))
})

test_that("require_implementation_license_notice() aborts when has_notice is not TRUE", {
  expect_error(require_implementation_license_notice(FALSE))
  expect_error(require_implementation_license_notice(NA))
})

test_that("require_cardpool_disclaimer() passes when has_disclaimer is TRUE", {
  expect_true(require_cardpool_disclaimer(TRUE))
})

test_that("require_cardpool_disclaimer() aborts when has_disclaimer is not TRUE", {
  expect_error(require_cardpool_disclaimer(FALSE))
  expect_error(require_cardpool_disclaimer(NA))
})

test_that("require_rules_disclaimer() passes when has_disclaimer is TRUE", {
  expect_true(require_rules_disclaimer(TRUE))
})

test_that("require_rules_disclaimer() aborts when has_disclaimer is not TRUE", {
  expect_error(require_rules_disclaimer(FALSE))
  expect_error(require_rules_disclaimer(NA))
})
