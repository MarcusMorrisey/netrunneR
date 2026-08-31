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

test_that("the map's surface colours are distinct from the page ground", {
  # There is no basemap. `map_water` is every pixel that is not land, and
  # `map_nodata` is land nobody has reported a tournament in. If either
  # equals the page ground the map loses its edge and reads as a hole cut
  # in the page rather than as a map.
  expect_named(NETRUNNER_MAP_SURFACE,
               c("map_water", "map_nodata", "map_edge", "map_point"))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", NETRUNNER_MAP_SURFACE)))
  expect_false(any(toupper(NETRUNNER_MAP_SURFACE) ==
                     toupper(NETRUNNER_PALETTE[["ground"]])))

  lum <- function(h) sum(grDevices::col2rgb(h)[, 1] * c(0.2126, 0.7152, 0.0722))
  # Land is lighter than sea, or the world is a negative of itself.
  expect_gt(lum(NETRUNNER_MAP_SURFACE[["map_nodata"]]),
            lum(NETRUNNER_MAP_SURFACE[["map_water"]]))
})

test_that("no-data grey is not a member of the ramp", {
  # "No data" and "a small number" must not be sayable in the same visual
  # language. The enforcement is that no-data comes from OUTSIDE the
  # scale -- if it ever drifted into the ramp, 131 countries would start
  # claiming a measurement nobody made.
  expect_false(toupper(NETRUNNER_MAP_SURFACE[["map_nodata"]]) %in%
                 toupper(NETRUNNER_MAP_RAMP))

  # And it is far enough from the ramp's floor to be told apart. The ramp
  # is amber; this is a neutral grey, so the gap is in hue as well, but
  # lightness alone has to carry it for a colour-blind reader.
  lum <- function(h) sum(grDevices::col2rgb(h)[, 1] * c(0.2126, 0.7152, 0.0722))
  expect_gt(abs(lum(NETRUNNER_MAP_RAMP[[1]]) -
                  lum(NETRUNNER_MAP_SURFACE[["map_nodata"]])), 20)
})

test_that("the app declares no tile provider at all", {
  # The reason this constant no longer exists. CartoDB.DarkMatter shipped
  # here and CARTO now stamps keyless tiles "API KEY REQUIRED" -- served
  # as HTTP 200 with a plausible byte count, so nothing in the stack
  # reports it and the words are simply drawn across the Atlantic.
  #
  # tmap ships `World` and the map draws that instead. This test is here
  # so a provider name cannot quietly come back.
  expect_false(exists("NETRUNNER_MAP_BASEMAPS",
                      envir = asNamespace("netrunneR"), inherits = FALSE))
  src <- readLines(test_path("..", "..", "R", "mod_meta_map.R"), warn = FALSE)
  expect_true(any(grepl("tm_basemap(NULL)", src, fixed = TRUE)))
  expect_false(any(grepl("cartocdn|CartoDB|arcgisonline", src)))
})


test_that("the venue bubbles do not compete with the choropleth", {
  # The ramp runs TO the accent, so amber bubbles sat on amber countries
  # and the two layers read as one -- which defeats a point layer whose
  # whole job is to be separable from the surface under it.
  point <- NETRUNNER_MAP_SURFACE[["map_point"]]
  expect_false(toupper(point) == toupper(NETRUNNER_PALETTE[["accent"]]))
  expect_false(toupper(point) %in% toupper(NETRUNNER_MAP_RAMP))

  # Separated on HUE, not only on lightness: that is what keeps the
  # bubbles legible over the bright end of the ramp as well as the dim.
  hue <- function(h) grDevices::rgb2hsv(grDevices::col2rgb(h))["h", 1]
  gap <- abs(hue(point) - hue(NETRUNNER_MAP_RAMP[[6]]))
  gap <- min(gap, 1 - gap)
  expect_gt(gap, 0.25)
})
