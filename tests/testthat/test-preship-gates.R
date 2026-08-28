# The point of these tests is NOT that the guards work -- it is that the
# gates are still shut. Every guard call site previously passed a literal
# TRUE, which made stopifnot(isTRUE(x)) unfalsifiable; nothing in the
# suite noticed, because a decorative guard passes exactly like a real
# one. These are the assertions that would have caught it.

test_that("a gate is only open if its notice actually renders", {
  # This replaced an assertion that the gates ship closed, which stopped
  # stating the invariant the moment they were legitimately attested.
  # What must hold in BOTH states is the conditional: an open gate is a
  # claim about rendered output, so if the claim is made the output has
  # to back it. Closing a gate again is allowed and does not fail here.
  if (isTRUE(SHIPPED_GATE_DEFAULTS$CARDPOOL_DISCLAIMER_CONFIRMED)) {
    txt <- squish(cardpool_disclaimer_ui())
    expect_match(txt, "copyrighted by Fantasy Flight Games and/or Wizards of the Coast")
    expect_match(txt, "Not maintained, produced, endorsed, supported, or affiliated")
  }
  if (isTRUE(SHIPPED_GATE_DEFAULTS$IMPLEMENTATION_MIT_NOTICE_CONFIRMED)) {
    txt <- squish(implementation_mit_notice_ui())
    expect_match(txt, "Permission is hereby granted, free of charge")
    expect_match(txt, "THE SOFTWARE IS PROVIDED")
  }
  succeed()
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

# A guard asserts "this view renders the notice". Nothing verified the
# second half of that sentence, so the app could satisfy every guard
# while rendering no notice at all -- and did: the matchup explorer named
# the MIT licence without reproducing it.

test_that("the cardpool disclaimer carries both COPYRIGHT.md clauses", {
  # Collapse whitespace: htmltools indents tag bodies, so a literal
  # match would be asserting the renderer's formatting, not the text.
  txt <- squish(cardpool_disclaimer_ui())
  expect_match(txt, "copyrighted by Fantasy Flight Games and/or Wizards of the Coast")
  expect_match(txt, "Not maintained, produced, endorsed, supported, or affiliated")
})

test_that("the implementation notice is an MIT notice, not the licence's name", {
  txt <- squish(implementation_mit_notice_ui())
  # The permission grant and the warranty disclaimer are the two parts
  # MIT actually requires to travel with the software. "MIT licensed" as
  # a bare phrase satisfies neither.
  expect_match(txt, "Permission is hereby granted, free of charge")
  expect_match(txt, "included in all copies or substantial portions")
  expect_match(txt, "THE SOFTWARE IS PROVIDED")
})

test_that("every guarded view actually renders the notice it asserts", {
  browser_ui  <- squish(mod_card_browser_ui("b"))
  matchup_ui  <- squish(mod_matchup_explorer_ui("m"))

  expect_match(browser_ui, "copyrighted by Fantasy Flight Games")
  expect_match(matchup_ui, "copyrighted by Fantasy Flight Games")
  # The matchup explorer is the implementation-sourced view, so it is the
  # one that must carry MIT.
  expect_match(matchup_ui, "Permission is hereby granted, free of charge")
})
