# The app's visual identity. Values are not invented here: they were
# measured off jinteki.net and netrunnerdb.com and recorded in
# homelab/docs/netrunneR/design-references/jinteki-netrunnerdb-visual-style.md,
# which also states the blend this app targets -- jinteki's black ground,
# blue-slate panels and amber accent, netrunnerdb's warm gray-tan text,
# Titillium Web, 4px radius, at a more comfortable type scale than
# jinteki's own cramped 12px.
#
# One theme object and one stylesheet, rather than inline styles in the
# modules: a colour that only exists inside a module is a colour nobody
# can change consistently later.

#' The measured palette, as named constants
#'
#' Exported so the values are quotable rather than retyped. Each is the
#' real value from the site named in the comment, not an approximation.
#' @format A character vector of hex/rgba colour values.
#' @export
NETRUNNER_PALETTE <- c(
  ground      = "#000000",              # jinteki.net body
  panel       = "rgba(40, 57, 77, 0.8)", # jinteki .blue-shade
  panel_solid = "#28394d",              # the same slate, opaque
  panel_edge  = "rgba(120, 146, 178, 0.28)",
  accent      = "#ffa600",              # jinteki buttons and focus rings
  ink         = "#c8c2bc",              # netrunnerdb.com body text
  ink_bright  = "#ffffff",              # netrunnerdb.com links
  ink_quiet   = "#8a8580"
)

#' The colours the map is made of
#'
#' TMAP HAS NO DARK MODE. There is no style to switch and no option to
#' set; a tmap map is dark because every part of it was told to be. These
#' are those parts.
#'
#' THERE IS NO BASEMAP, and that is the first decision. Every tile
#' provider worth having now meters keyless requests, and does it
#' silently: CartoDB.DarkMatter returns HTTP 200, the right number of
#' bytes, and a picture with "API KEY REQUIRED" stamped across the
#' Atlantic. Nothing in the stack reports that -- the map renders and the
#' tile count is correct. Swapping to Esri bought a basemap that works
#' today, from a provider under the same commercial pressure, in exchange
#' for a URL template, an attribution obligation tmap has no argument
#' for, and a network round trip on every view.
#'
#' So the tiles are gone. tmap already ships `World`: 177 country
#' polygons, offline, and already loaded for the join the choropleth
#' needs. The map draws all of them in `map_nodata` and paints the ones
#' with tournaments over the top. That is the whole basemap, and it is
#' the right one for this package -- an offline mirror whose entire
#' argument is that it does not depend on a service being up should not
#' have been reaching across the internet to draw its own map.
#'
#' It costs city labels, coastline detail, and anything below country
#' level. This is a country-level choropleth with a venue-point layer, so
#' none of those was carrying information that is now missing.
#'
#' THE RAMP RUNS TO THE ACCENT. A single hue interpolated from a mid
#' amber-brown to the app's own `accent`, so the brightest class is the
#' colour the rest of the app already uses for "this one matters".
#' Computed once with colorRampPalette() and written down rather than
#' recomputed at draw time, so the bands are quotable and a reviewer can
#' see them. Single-hue, not a rainbow: the value it encodes is one
#' ordered quantity, and a ramp that changes hue invites a reader to see
#' categories in it.
#'
#' ITS FLOOR IS DELIBERATELY NOT DARK. The obvious ramp starts near black
#' and is the wrong one here: the lowest class has to be legible against
#' `map_nodata`, and a near-black band next to a grey one says "almost
#' nothing measured" where the neighbouring country's grey says "nothing
#' measured at all". Those are different claims and must not look alike.
#' The floor was lifted from #4A3714 to #6E5321 for exactly that reason.
#'
#' `map_water` is the sea, `map_nodata` is land nobody has reported a
#' tournament in, and `map_edge` is the hairline between countries. All
#' three are deliberately DIFFERENT from `ground`: a map whose sea is the
#' same black as the page behind it has no edge, and reads as a hole
#' rather than as a map.
#'
#' `map_nodata` is also deliberately OUTSIDE the ramp. "No data" and "a
#' small number" must not be sayable in the same visual language, and a
#' colour drawn from a different scale is how that is enforced rather
#' than merely intended.
#' @format A character vector of six hex values, dim to bright.
#' @export
NETRUNNER_MAP_RAMP <- c(
  "#6E5321", "#8B6928", "#A87F30", "#C59537", "#E2AB3F", "#FFC247"
)

#' @rdname NETRUNNER_MAP_RAMP
#' @format A named character vector of the map's non-data colours.
#' @export
NETRUNNER_MAP_SURFACE <- c(
  # Deep slate rather than the page's black, so the map has an edge.
  map_water  = "#0C1218",
  # Land with no reported tournament. A NEUTRAL grey, from outside the
  # ramp -- see above.
  map_nodata = "#2A2F36",
  # Just visible against map_nodata, and against nothing else.
  map_edge   = "#171B21"
)

#' The app's base font, degrading rather than failing
#'
#' Titillium Web is served locally (bslib downloads and hosts it) rather
#' than linked from Google's CDN, so viewers make no third-party request
#' to read a card list -- the same instinct behind this package's other
#' external-dependency gates.
#'
#' That fetch needs network at app start, and an app whose whole purpose
#' is an OFFLINE mirror should not fail to boot because a font host is
#' unreachable. On any error this falls back to a stack naming the same
#' face first, so a viewer who has it installed still sees the intended
#' typography and everyone else sees a clean sans.
#'
#' @return A bslib font object, or a character font stack.
#' @keywords internal
app_font <- function() {
  tryCatch(
    bslib::font_google("Titillium Web", local = TRUE),
    error = function(e) {
      rlang::inform(
        paste("Could not fetch Titillium Web; falling back to a system font stack.",
              "The theme is otherwise unaffected."),
        class = "netrunneR_font_fallback"
      )
      c("Titillium Web", "Helvetica Neue", "Arial", "sans-serif")
    }
  )
}

#' The ICE::BREAKER bslib theme
#'
#' Everything expressible as a Bootstrap variable lives here; everything
#' that needs a selector lives in inst/shiny-app/www/netrunner.css. The
#' split matters because bslib variables cascade into components this app
#' never names directly, while raw CSS does not.
#'
#' @return A bslib theme object.
#' @export
netrunner_theme <- function() {
  p <- NETRUNNER_PALETTE
  bslib::bs_theme(
    version = 5,
    bg = p[["ground"]],
    fg = p[["ink"]],
    primary = p[["accent"]],
    base_font = app_font(),
    # 14px, deliberately above jinteki's 12px: the reference notes their
    # scale reads as cramped and says not to copy it.
    "font-size-base" = "0.875rem",
    # 4px, matching both reference sites.
    "border-radius" = "0.25rem",
    "link-color" = p[["ink_bright"]],
    "body-bg" = p[["ground"]],
    "body-color" = p[["ink"]]
  )
}
