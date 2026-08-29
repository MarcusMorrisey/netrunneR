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
    txt <- squish(shiny::tagList(cardpool_disclaimer_ui()))
    # The default is the mixed-pool notice: all three in one disjunction.
  # Null Signal Games is named even though the cardpool source's own
  # COPYRIGHT.md does not name them -- see cardpool_disclaimer_ui() for
  # the card that settles it.
  expect_match(txt, "copyrighted by Null Signal Games, Fantasy Flight Games, and/or Wizards of the Coast")
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
  txt <- squish(shiny::tagList(cardpool_disclaimer_ui()))
  # The default is the mixed-pool notice: all three in one disjunction.
  # Null Signal Games is named even though the cardpool source's own
  # COPYRIGHT.md does not name them -- see cardpool_disclaimer_ui() for
  # the card that settles it.
  expect_match(txt, "copyrighted by Null Signal Games, Fantasy Flight Games, and/or Wizards of the Coast")
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
  board_ui    <- squish(mod_lane_board_ui("l"))

  expect_match(browser_ui, "copyrighted by Null Signal Games")
  expect_match(matchup_ui, "copyrighted by Null Signal Games")
  expect_match(board_ui, "copyrighted by Null Signal Games")
  # The matchup explorer and the lane board both render
  # implementation-derived cost_to_break/credit_differential, so both must
  # carry MIT. The lane board is the app's landing screen, which makes it
  # the one most likely to be restyled without the notice being noticed --
  # exactly what this gate exists to catch.
  expect_match(matchup_ui, "Permission is hereby granted, free of charge")
  expect_match(board_ui, "Permission is hereby granted, free of charge")
})

test_that("an open nrdb gate means the disclaimer actually renders", {
  # The gate is an attestation that the app SHOWS the disclaimer, not that
  # someone once read the terms. If it is open, the rulings panel must
  # carry the text -- otherwise the attestation is about nothing, which is
  # the exact failure these gates exist to catch.
  if (!isTRUE(SHIPPED_GATE_DEFAULTS$NRDB_ATTRIBUTION_CONFIRMED)) {
    succeed()
    return(invisible(NULL))
  }

  rulings <- data.frame(
    title = "Some Card", ruling = "A ruling. [Official FAQ]",
    date_update = "2020-01-01", nsg_rules_team_verified = 0L,
    stringsAsFactors = FALSE
  )
  panel <- squish(shiny::tagList(card_rulings_ui(rulings, "Some Card")))

  expect_match(panel, "copyrighted by Fantasy Flight Games and/or Null Signal Games")
  expect_match(panel, "not produced, endorsed, supported, or affiliated")
  expect_match(panel, "netrunnerdb.com")
})

test_that("the nrdb disclaimer quotes the source's own two statements", {
  txt <- squish(nrdb_disclaimer_ui())
  # Verbatim from netrunnerdb.com/en/about. Both clauses, not just the
  # non-affiliation half -- the same omission the cardpool disclaimer was
  # once corrected for.
  expect_match(txt, "copyrighted by Fantasy Flight Games and/or Null Signal Games")
  expect_match(txt, "not produced, endorsed, supported, or affiliated with Fantasy Flight Games")
})

test_that("the rules gate stays closed until a rules-sourced view exists", {
  # RULES_DISCLAIMER_CONFIRMED guards a view nobody has built. Attesting
  # that the app renders a disclaimer it has nowhere to render is the
  # failure this catches -- the gates were added precisely because a
  # guard with nothing behind it still passes.
  rendered_anywhere <- any(grepl(
    "require_rules_disclaimer",
    unlist(lapply(list.files(test_path("..", ".."), pattern = "[.]R$",
                             recursive = TRUE, full.names = TRUE),
                  readLines, warn = FALSE))
  ))
  if (!isTRUE(SHIPPED_GATE_DEFAULTS$RULES_DISCLAIMER_CONFIRMED)) {
    succeed()
  } else {
    expect_true(rendered_anywhere)
  }
})

# ---- per-publisher copyright notices -------------------------------
# The licence changed hands: Fantasy Flight cards carry an FFG/Wizards
# line on the card face, Null Signal cards carry a Null Signal line and
# no FFG line at all. One merged sentence would credit the wrong
# publisher for half the pool in each direction.

mini_legality <- function() {
  list(
    printing = data.frame(code = c("01061", "36028"), card_id = c("a", "b"),
                          card_set_id = c("core", "vp"), stringsAsFactors = FALSE),
    card_set = data.frame(id = c("core", "vp"), card_cycle_id = c("cy_ffg", "cy_nsg"),
                          stringsAsFactors = FALSE),
    card_cycle = data.frame(id = c("cy_ffg", "cy_nsg"),
                            released_by = c("fantasy_flight_games", "null_signal_games"),
                            stringsAsFactors = FALSE)
  )
}

test_that("a card is traced to the publisher that released it", {
  leg <- mini_legality()
  expect_equal(card_publishers("01061", leg), "fantasy_flight_games")
  expect_equal(card_publishers("36028", leg), "null_signal_games")
  expect_setequal(card_publishers(c("01061", "36028"), leg),
                  c("fantasy_flight_games", "null_signal_games"))
})

test_that("a Null Signal card is not credited to Fantasy Flight", {
  txt <- squish(cardpool_disclaimer_ui("null_signal_games"))
  expect_match(txt, "copyrighted by Null Signal Games[.]")
  expect_no_match(txt, "Fantasy Flight")
  expect_no_match(txt, "Wizards of the Coast")
})

test_that("a Fantasy Flight card keeps COPYRIGHT.md's wording exactly", {
  # That file was never wrong, only incomplete in scope. Applied to the
  # cards it actually describes, it is accurate verbatim.
  txt <- squish(cardpool_disclaimer_ui("fantasy_flight_games"))
  expect_match(txt, "copyrighted by Fantasy Flight Games and/or Wizards of the Coast")
  expect_match(txt, paste("Not maintained, produced, endorsed, supported, or affiliated",
                          "with Fantasy Flight Games and/or Wizards of the Coast"))
  expect_no_match(txt, "Null Signal")
})

test_that("a mixed pool makes one claim, not two contradictory ones", {
  # "Card data is copyrighted by Null Signal Games" and "Card data is
  # copyrighted by Fantasy Flight Games and/or Wizards of the Coast"
  # cannot both be true of the same card data. Printed together with
  # nothing saying each covers a subset, they contradict; one disjunction
  # over all three holds for every card in the pool.
  txt <- squish(cardpool_disclaimer_ui(c("null_signal_games", "fantasy_flight_games")))
  expect_match(txt, "copyrighted by Null Signal Games, Fantasy Flight Games, and/or Wizards of the Coast")
  expect_equal(length(gregexpr("Card data is copyrighted", txt)[[1]]), 1L)
})

test_that("an untraceable card still gets a notice rather than silence", {
  # Missing legality tables must not mean a view renders no copyright
  # line at all -- that is the failure the gates exist to prevent. It
  # falls back to naming all three, which is true of any card.
  expect_equal(card_publishers("01061", NULL), character(0))
  txt <- squish(cardpool_disclaimer_ui(character(0)))
  expect_match(txt, "copyrighted by Null Signal Games, Fantasy Flight Games, and/or Wizards of the Coast")
})
