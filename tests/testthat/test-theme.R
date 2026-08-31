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


test_that("the map ramp is six hex values, dim to bright", {
  expect_length(NETRUNNER_MAP_RAMP, 6L)
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", NETRUNNER_MAP_RAMP)))

  # Monotonically lighter. A sequential scale whose lightness wanders
  # encodes its ordering in nothing a reader can see.
  lum <- vapply(NETRUNNER_MAP_RAMP, function(h) {
    rgb <- grDevices::col2rgb(h)[, 1]
    sum(rgb * c(0.2126, 0.7152, 0.0722))
  }, numeric(1))
  expect_true(all(diff(lum) > 0))

  # It ends on the app's accent hue rather than an unrelated yellow.
  expect_equal(toupper(NETRUNNER_MAP_RAMP[[6]]), "#FFC247")
})

test_that("the ramp's floor clears the ground it is drawn on", {
  # Countries with no data are NOT drawn -- they fall through to the
  # basemap. So a near-black lowest class would be indistinguishable from
  # "no data", which is a different claim entirely. This is the guard on
  # that: every band has to be visibly lighter than the page ground.
  ground <- grDevices::col2rgb(unname(NETRUNNER_PALETTE[["ground"]]))[, 1]
  floor_col <- grDevices::col2rgb(NETRUNNER_MAP_RAMP[[1]])[, 1]
  expect_true(sum(floor_col) - sum(ground) > 150)
})

test_that("the dark basemap is the default, and the light ones stay", {
  # Order is the decision: leaflet shows the first as the default layer.
  expect_match(names(NETRUNNER_MAP_BASEMAPS)[[1]], "Dark")
  # Kept, not removed. A dark map is the wrong answer for some viewers
  # and every printer, and enforcing a look by deleting the choice is a
  # worse trade than offering it.
  expect_true(length(NETRUNNER_MAP_BASEMAPS) > 1)
  expect_true(any(grepl("Light|OpenStreetMap", names(NETRUNNER_MAP_BASEMAPS))))
})

test_that("every basemap is named, or the layer control comes out empty", {
  # tmap can label a provider name by itself and a URL template not at
  # all. Mix the two unnamed and it labels NEITHER -- the base list in
  # addLayersControl arrives empty and the switcher is unusable.
  expect_false(is.null(names(NETRUNNER_MAP_BASEMAPS)))
  expect_true(all(nzchar(names(NETRUNNER_MAP_BASEMAPS))))
})

test_that("no basemap comes from a provider that meters keyless tiles", {
  # CartoDB.DarkMatter shipped here first and CARTO now stamps keyless
  # tiles "API KEY REQUIRED" -- served as HTTP 200 with a plausible byte
  # count, so nothing in the stack reports a problem and the words are
  # simply drawn across the world. Nothing automated caught it and this
  # test would not have either; it is here so the name cannot come back
  # by accident.
  expect_false(any(grepl("cartocdn|CartoDB", NETRUNNER_MAP_BASEMAPS)))
})

test_that("a raw-URL basemap is given the credit it cannot bring itself", {
  # A named provider carries an attribution string from
  # leaflet-providers; a URL template carries nothing, and tmap has no
  # argument to supply one.
  #
  # THE FIXTURE IS A REAL WIDGET, not a hand-written imitation of one.
  # The first version of this test built the call list from the same
  # assumption the function made -- that `addTiles` records attribution
  # in its second argument -- so the two agreed with each other and both
  # were wrong. leaflet folds attribution into options$attribution and
  # records (urlTemplate, layerId, group, options); the second slot is
  # layerId. The test passed, the map rendered, and the attribution
  # control said only "Leaflet".
  skip_if_not_installed("tmap")
  skip_if_not_installed("sf")
  skip_if_not_installed("leaflet")

  tmap::tmap_mode("view")
  e <- new.env(parent = emptyenv())
  utils::data("World", package = "tmap", envir = e)
  lf <- tmap::tmap_leaflet(
    tmap::tm_basemap(NETRUNNER_MAP_BASEMAPS) +
      tmap::tm_shape(get("World", envir = e)) + tmap::tm_polygons()
  )

  before <- Filter(function(cl) identical(cl$method, "addTiles"), lf$x$calls)
  expect_gt(length(before), 0)
  expect_null(before[[1]]$args[[4]]$attribution)

  out <- attribute_basemap_tiles(lf, attribution = "CREDIT")
  tiles <- Filter(function(cl) identical(cl$method, "addTiles"), out$x$calls)
  expect_equal(tiles[[1]]$args[[4]]$attribution, "CREDIT")

  # Provider tiles are left alone -- they bring their own from
  # leaflet-providers, and overwriting one would replace a correct credit
  # with a wrong one.
  providers <- Filter(function(cl) identical(cl$method, "addProviderTiles"), out$x$calls)
  expect_gt(length(providers), 0)
  expect_false(any(vapply(providers, function(cl) {
    identical(cl$args[[4]]$attribution, "CREDIT")
  }, logical(1))))
})

test_that("an existing credit is never overwritten", {
  fake <- list(x = list(calls = list(
    list(method = "addTiles",
         args = list("https://x/{z}/{y}/{x}", "id", "grp",
                     list(attribution = "already set")))
  )))
  out <- attribute_basemap_tiles(fake, attribution = "CREDIT")
  expect_equal(out$x$calls[[1]]$args[[4]]$attribution, "already set")
})
