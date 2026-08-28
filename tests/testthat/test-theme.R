# The visual identity is measured, not invented -- see
# design-references/jinteki-netrunnerdb-visual-style.md. These tests pin
# the measured values so a later "tidy up the colours" cannot quietly
# replace them with something plausible.

test_that("the palette holds the measured values, not approximations", {
  expect_equal(unname(NETRUNNER_PALETTE[["ground"]]), "#000000")
  expect_equal(unname(NETRUNNER_PALETTE[["accent"]]), "#ffa600")
  expect_equal(unname(NETRUNNER_PALETTE[["ink"]]), "#c8c2bc")
  expect_equal(unname(NETRUNNER_PALETTE[["panel"]]), "rgba(40, 57, 77, 0.8)")
})

test_that("netrunner_theme() carries the palette into Bootstrap", {
  th <- netrunner_theme()
  expect_s3_class(th, "bs_theme")
  vars <- bslib::bs_get_variables(th, c("body-bg", "body-color", "primary", "border-radius"))
  expect_equal(unname(tolower(vars[["body-bg"]])), "#000000")
  expect_equal(unname(tolower(vars[["primary"]])), "#ffa600")
  expect_equal(unname(vars[["border-radius"]]), "0.25rem")
})

test_that("app_font() degrades instead of taking the app down", {
  # An offline mirror that cannot start because a font host is
  # unreachable would be a poor trade. The fallback still names the
  # intended face first, so a viewer who has it installed sees it.
  testthat::local_mocked_bindings(
    font_google = function(...) stop("no network"), .package = "bslib"
  )
  expect_message(fallback <- app_font(), class = "netrunneR_font_fallback")
  expect_type(fallback, "character")
  expect_equal(fallback[[1]], "Titillium Web")
  expect_true("sans-serif" %in% fallback)
})

test_that("the card grid is themeable: classes, not inline styles", {
  cards <- mini_pool_cardpool()
  selected_code <- shiny::reactiveVal(NULL)

  shiny::testServer(
    mod_card_browser_server, args = list(cards = cards, selected_code = selected_code),
    {
      session$flushReact()
      html <- as.character(output$card_grid$html)
      expect_match(html, "nr-grid", fixed = TRUE)
      expect_match(html, "nr-card", fixed = TRUE)
      # The sizing that used to be inline must not have come back.
      expect_false(grepl("width: 150px", html, fixed = TRUE))
    }
  )
})
