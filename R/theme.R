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

#' The choropleth ramp, and the basemaps under it
#'
#' TMAP HAS NO DARK MODE. There is no style to switch and no option to
#' set; a tmap map is dark because each of its parts was told to be. So
#' this is three separate decisions, kept together because they only work
#' together.
#'
#' THE RAMP RUNS TO THE ACCENT. A single hue interpolated from a mid
#' amber-brown to the app's own `accent`, so the brightest class is the
#' colour the rest of the app already uses for "this one matters".
#' Computed once with colorRampPalette() and written down rather than
#' recomputed at draw time, so the bands are quotable and a reviewer can
#' see them.
#'
#' ITS FLOOR IS DELIBERATELY NOT DARK. The obvious ramp starts near black
#' and it is the wrong one here, because countries with NO data are not
#' drawn at all -- they fall through to the basemap (see
#' build_tournament_map()). A near-black lowest class is therefore not
#' merely dim, it is indistinguishable from "we have nothing for this
#' country", which is a different claim entirely. The floor was lifted
#' from #4A3714 to #6E5321 for exactly that reason: every drawn country
#' has to read as drawn.
#'
#' Single-hue, not a rainbow: the value it encodes is one ordered
#' quantity, and a ramp that changes hue invites a reader to see
#' categories in it.
#'
#' THE DARK BASEMAP IS FIRST, NOT ONLY. It is the default because the
#' page around it is black. The two light basemaps stay in the layer
#' control because a dark map is the wrong answer for some viewers and
#' every printer, and removing the choice to enforce a look is a worse
#' trade than offering it.
#'
#' IT IS AN ESRI URL, NOT CartoDB.DarkMatter, AND THAT IS THE WHOLE
#' STORY. DarkMatter is the obvious choice, is in leaflet-providers, and
#' shipped here first -- and CARTO now serves keyless requests as tiles
#' stamped "API KEY REQUIRED" across the middle. They return HTTP 200 and
#' the correct number of bytes, so nothing in the stack reports a
#' problem: the map renders, the tile count is right, and the words are
#' simply drawn on the world. It was caught by looking at the picture.
#'
#' Esri's World_Dark_Gray_Base needs no key and is the same family as the
#' Esri.WorldGrayCanvas this map already trusted. leaflet-providers has
#' no entry for it -- it lists ten Esri basemaps and none of the dark
#' ones -- so it goes in as a URL template, which is why the vector is
#' NAMED. Mixing an unnamed URL with provider names leaves tmap unable to
#' label either, and the layer control comes out empty.
#'
#' A raw URL also carries no attribution, where a named provider brings
#' its own; see attribute_basemap_tiles() for the half of that this file
#' cannot do.
#'
#' ESRI'S DARK IS LIGHTER THAN CARTO'S, and that changes what the map
#' says. DarkMatter draws land at roughly #1a1a1a, near enough to this
#' page's ground that a country with no tournaments simply disappeared.
#' Esri's draws it at roughly #3a3a3a, so undrawn countries read as
#' geography -- present, unmeasured -- instead of as absence. That is the
#' better of the two for a map whose whole point is that some countries
#' have data and most do not, so the substitution is an improvement
#' rather than a compromise. It does mean the choropleth now has to
#' separate itself from a grey rather than from black, which the amber
#' ramp does on hue as well as on lightness.
#' @format A character vector of six hex values, dim to bright.
#' @export
NETRUNNER_MAP_RAMP <- c(
  "#6E5321", "#8B6928", "#A87F30", "#C59537", "#E2AB3F", "#FFC247"
)

#' @rdname NETRUNNER_MAP_RAMP
#' @format A character vector of leaflet provider names, default first.
#' @export
NETRUNNER_MAP_BASEMAPS <- c(
  "Dark (Esri)" = paste0(
    "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/",
    "World_Dark_Gray_Base/MapServer/tile/{z}/{y}/{x}"
  ),
  "Light (Esri)"  = "Esri.WorldGrayCanvas",
  "OpenStreetMap" = "OpenStreetMap"
)

#' The credit a raw tile URL does not carry for itself
#'
#' leaflet-providers ships an attribution string with every named
#' provider, and leaflet renders it into the corner control without
#' anyone asking. A URL template has none -- so the dark basemap would
#' have drawn Esri's tiles with Esri's name nowhere on the page.
#'
#' That is the same obligation the alwaysberunning.net backlink
#' discharges for the tournament data, and it is met the same way: named,
#' visible, and not conditional on anything working.
#' @format A single string.
#' @export
NETRUNNER_BASEMAP_ATTRIBUTION <- paste(
  "Tiles &copy; Esri &mdash; Esri, HERE, Garmin,",
  "&copy; OpenStreetMap contributors, and the GIS user community"
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
