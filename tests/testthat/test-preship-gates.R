# The point of these tests is NOT that the guards work -- it is that the
# gates are still shut. Every guard call site previously passed a literal
# TRUE, which made stopifnot(isTRUE(x)) unfalsifiable; nothing in the
# suite noticed, because a decorative guard passes exactly like a real
# one. These are the assertions that would have caught it.

test_that("the pre-ship gates ship closed", {
  # Read from the capture taken before helper-preship-gates.R opened them
  # for the suite -- the runtime bindings are TRUE here by design.
  #
  # Flipping either default to TRUE is a human attestation about upstream
  # terms. If this fails, that attestation is what needs checking; do not
  # "fix" it by editing the expectation.
  expect_false(SHIPPED_GATE_DEFAULTS$CARDPOOL_DISCLAIMER_CONFIRMED)
  expect_false(SHIPPED_GATE_DEFAULTS$IMPLEMENTATION_MIT_NOTICE_CONFIRMED)
})

test_that("a closed gate blocks its guarded view rather than degrading", {
  # Guards are stopifnot() assertions: a view rendering cardpool content
  # without its disclaimer must abort, not render quietly.
  expect_error(require_cardpool_disclaimer(FALSE))
  expect_error(require_implementation_license_notice(FALSE))
})

test_that("the guards reject every non-TRUE argument, not just FALSE", {
  # isTRUE(), not truthiness: NA and 1 are what a careless caller reaches
  # for, and both would otherwise read as "confirmed".
  for (bad in list(FALSE, NA, NULL, 1, "yes", c(TRUE, TRUE))) {
    expect_error(require_cardpool_disclaimer(bad))
    expect_error(require_implementation_license_notice(bad))
  }
})

test_that("no guard call site passes a literal TRUE", {
  # The regression that motivated the gates, and the one no behavioral
  # test can catch: a re-hardcoded call site makes the view work.
  r_dir <- test_path("..", "..", "R")
  skip_if_not(dir.exists(r_dir), "R/ source not resolvable from installed package")

  sources <- list.files(r_dir, pattern = "[.]R$", full.names = TRUE)
  offenders <- Filter(
    function(f) any(grepl("require_(cardpool_disclaimer|implementation_license_notice)[(]TRUE[)]",
                          readLines(f, warn = FALSE))),
    sources
  )
  expect_equal(basename(offenders), character(0))
})
