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

test_that("ensure_graphics_device() opens a null device when none is open", {
  # grDevices::pdf(file = NULL) is what tmap_leaflet()'s internal grid
  # measurement needs and nothing else here uses, so closing whatever
  # this leaves open is safe cleanup rather than a side effect a later
  # test could depend on.
  while (!is.null(grDevices::dev.list())) grDevices::dev.off()

  opened <- ensure_graphics_device()

  expect_true(opened)
  expect_false(is.null(grDevices::dev.list()))

  grDevices::dev.off()
})

test_that("ensure_graphics_device() leaves an already-open device alone", {
  grDevices::pdf(file = NULL)
  before <- grDevices::dev.cur()

  opened <- ensure_graphics_device()

  expect_false(opened)
  expect_identical(grDevices::dev.cur(), before)

  grDevices::dev.off()
})
