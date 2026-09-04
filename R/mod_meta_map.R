#' The tournament map
#'
#' A country choropleth of tournaments per million people, with a point
#' layer of venue density over it. Adapted from the mapping notebook that
#' already existed, so the composition is the one that was already
#' wanted; what changed is where the data comes from.
#'
#' SF AND TMAP ARE SUGGESTS, NOT IMPORTS, and this module is why. In
#' Imports the whole package fails to load anywhere the spatial stack is
#' absent -- pkgload aborts before a single test runs -- which couples
#' the package to an image rebuild for a view most sessions never open.
#' So every entry point here checks and degrades, in the same way
#' card_rulings_ui() degrades without an nrdb release.
#'
#' @param id Module id.
#' @export
mod_meta_map_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(
    class = "nr-map-page",
    shiny::div(class = "nr-map-head",
      shiny::h4("Tournament map"),
      shiny::uiOutput(ns("summary"))
    ),
    shiny::uiOutput(ns("map_slot")),
    # ABR's terms require a backlink, and it renders whether or not the
    # map above it managed to draw -- the obligation attaches to using
    # the data, not to the drawing succeeding.
    abr_attribution_ui(),
    shiny::uiOutput(ns("notes"))
    )
  )
}

#' Tournament map module server
#'
#' @param id Module id.
#' @param tournaments The abr `tournament` table, or NULL when no abr
#'   release is active. NULL renders an explanation rather than an empty
#'   map, for the reason given on mod_card_detail_server()'s `rulings`.
#' @param filters A reactive returning the app-level filter selection,
#'   from mod_filter_bar_server(). NULL filters nothing, which is what a
#'   caller with no abr release has to be able to ask for.
#' @param identities The cardpool identity cards, used only to recognise
#'   draft identities so the filter can exclude them.
#' @export
mod_meta_map_server <- function(id, tournaments = NULL, filters = NULL,
                                identities = NULL) {
  shiny::moduleServer(id, function(input, output, session) {

    # The gate, passed the constant and never a literal TRUE. With it
    # closed this view aborts, which is the intended behaviour: the
    # backlink above is what the attestation is about, and nobody but a
    # person can attest that it renders.
    require_abr_attribution(ABR_ATTRIBUTION_CONFIRMED)

    have_spatial <- requireNamespace("sf", quietly = TRUE) &&
      requireNamespace("tmap", quietly = TRUE) &&
      requireNamespace("leaflet", quietly = TRUE)

    # VIEW MODE, or there is no map. tmap defaults to "plot", which
    # renders a static image -- and tmapOutput() then builds a
    # plotOutput() rather than a leaflet widget, which fails outright
    # with "invalid width argument" inside a fluid layout. The map is
    # meant to be panned and zoomed, so view mode is not a preference
    # here, it is the thing working at all.
    if (have_spatial) tmap::tmap_mode("view")

    dated <- with_parsed_dates(tournaments)

    # Everything below reads THIS, so the slider and the map cannot
    # disagree about which tournaments are in view -- and because
    # `selected` comes from the app rather than from here, neither can
    # this view and the stats view.
    draft_codes <- draft_identity_codes(identities)
    in_range <- shiny::reactive({
      apply_tournament_filters(
        dated, if (is.null(filters)) NULL else filters(), draft_codes
      )
    })

    shaped <- shiny::reactive({
      d <- in_range()
      if (is.null(d) || !nrow(d)) return(NULL)
      world <- world_polygons()
      tournament_country_counts(
        d,
        map_names = if (is.null(world)) NULL else world$name
      )
    })

    output$summary <- shiny::renderUI({
      safe_render(function() {
        s <- shaped()
        if (is.null(s)) return(NULL)
        v <- tournament_venues(in_range())
        shiny::tags$p(class = "small text-muted", sprintf(
          "%s tournaments across %s countries%s.",
          format(sum(s$counts$tournaments), big.mark = ","),
          nrow(s$counts),
          if (nrow(v)) sprintf(", %s distinct venues", format(nrow(v), big.mark = ",")) else ""
        ))
      })
    })

    output$map_slot <- shiny::renderUI({
      safe_render(function() {
        if (!have_spatial) {
          return(alert_box(paste(
            "The map needs the sf, tmap and leaflet packages, which are not installed",
            "in this environment. Everything else on this page still reads",
            "the same data."
          ), "info"))
        }
        if (is.null(shaped())) {
          return(alert_box(
            "No active abr release, so there are no tournaments to map.", "info"
          ))
        }
        leaflet::leafletOutput(session$ns("map"), height = "560px")
      })
    })

    if (have_spatial) {
      # leaflet::renderLeaflet() over tmap_leaflet(), NOT tmap::renderTmap().
      #
      # renderTmap() routes through print.tmap(), which computes scale
      # defaults against the output device -- and inside Shiny that failed
      # with "missing value where TRUE/FALSE needed" from
      # get_scale_defaults(), while the identical map object printed
      # perfectly in a plain R session and in shiny::testServer(). A
      # failure that appears only in the running app and not in any test
      # is worth routing around rather than living with.
      #
      # tmap_leaflet() converts the same object to a leaflet widget
      # directly, so the composition below is still entirely tmap's; only
      # the handoff to Shiny changes.
      #
      # safe_render() is NOT used here. It returns an alert_box() tag on
      # error, and a widget renderer cannot display a shiny.tag -- the
      # same trap documented on mod_matchup_explorer_ui()'s separate
      # status slot. The message belongs in the status output above,
      # which has one.
      output$map <- leaflet::renderLeaflet({
        s <- shaped()
        shiny::req(!is.null(s))
        tmap::tmap_leaflet(
          build_tournament_map(s$counts, tournament_venues(in_range()))
        )
      })
    }

    output$notes <- shiny::renderUI({
      safe_render(function() {
        s <- shaped()
        if (is.null(s)) return(NULL)
        v <- tournament_venues(in_range())

        shiny::tagList(
          # A country the map cannot place is invisible once drawn, and
          # invisible looks exactly like "no tournaments here". Named
          # rather than dropped.
          if (length(s$unmatched)) {
            # Distinguishes "this basemap doesn't carry a polygon this
            # small" from "the name didn't match" -- an earlier version
            # of the underlying rename table tried to fix this class of
            # gap by guessing spellings, and the guesses broke two
            # countries that already matched (see country_map_name()).
            # This reads as a basemap resolution limit because that is
            # what it is, not an invitation to try another rename.
            alert_box(sprintf(
              paste(
                "Not drawn: %s. Small states and territories a world map",
                "at this scale doesn't carry -- not a spelling mismatch,",
                "and not something renaming these names would fix."
              ),
              paste(s$unmatched, collapse = ", ")
            ), "warning")
          },
          # The point layer is absent on a release built before venue
          # coordinates were admitted. Saying so beats a map that looks
          # complete and is missing half its composition.
          if (!nrow(v)) {
            alert_box(paste(
              "No venue points: the active abr release predates venue",
              "coordinates. The country layer is unaffected; the points",
              "appear after the next abr sync."
            ), "info")
          }
        )
      })
    })
  })
}

#' The world polygons the choropleth draws on
#'
#' Wrapped so the module has one place to fail gracefully. tmap ships
#' `World` as a dataset; loading it by name into a local environment
#' rather than the global one keeps utils::data()'s side effect where it
#' can be reasoned about.
#'
#' @return An sf data frame with `name` and `pop_est`, or NULL when tmap
#'   is unavailable.
#' @keywords internal
world_polygons <- function() {
  if (!requireNamespace("tmap", quietly = TRUE)) return(NULL)
  e <- new.env(parent = emptyenv())
  utils::data("World", package = "tmap", envir = e)
  get("World", envir = e)
}

#' Draw the tournament map
#'
#' Kept apart from the module so the composition can be built and
#' inspected without a Shiny session.
#'
#' PER MILLION, NOT RAW COUNTS. A raw-count choropleth of anything is a
#' population map wearing a different label -- the United States would be
#' darkest because it is large, which says nothing about Netrunner.
#' Dividing by population asks the question actually worth asking: where
#' is the game played, relative to how many people there are to play it.
#'
#' The raw count rides along in the hover, because per-million is
#' unstable for a small country -- one tournament in a country of two
#' million outranks two hundred in the United States -- and a reader
#' needs to see the numerator to know that.
#'
#' @param counts A data frame from tournament_country_counts()$counts.
#' @param venues A data frame from tournament_venues().
#' @return A tmap object.
#' @keywords internal
build_tournament_map <- function(counts, venues) {
  world <- world_polygons()

  # AN INNER JOIN, still: countries with no tournaments are not in the
  # CHOROPLETH. They are drawn now -- see the base layer below -- but in
  # a neutral grey that is not a member of the ramp.
  #
  # That distinction is the whole point, and it is why the base layer
  # does not simply zero-fill. An earlier version kept every country and
  # set its count to 0, which put about 130 countries in the palette's
  # lowest band: that says "we measured here and found almost nothing"
  # where the truth is that nobody reports tournaments there, and it
  # drowns the countries that do have data in a wash of near-identical
  # colour. "No data" and "a small number" must not be sayable in the
  # same visual language; giving one of them a colour from outside the
  # scale is how that is enforced.
  joined <- merge(world, counts, by = "name", all.x = FALSE)

  pop <- as.numeric(joined$pop_est)
  joined$per_million <- ifelse(
    is.na(pop) | pop <= 0, NA_real_,
    round((joined$tournaments / pop) * 1e6, 3)
  )

  # NAMED EXPLICITLY. tmap labels a layer after the object handed to
  # tm_shape(), so without this the layer control offered the reader
  # "joined" and "pts" -- the local variable names in this function,
  # which mean something to whoever wrote it and nothing to anyone
  # looking at a map.
  # NO TILES AT ALL. tm_basemap(NULL) is what removes them; leave it out
  # and tmap helpfully supplies three of its own, one of which arrives
  # watermarked.
  #
  # The world is drawn instead, from the same `World` object the join
  # above already needed: all 177 countries in the neutral no-data grey,
  # with the 46 that have tournaments painted over them. Geography with
  # no network call, no provider, no key and no attribution obligation.
  #
  # group.control = "none" keeps it out of the layer switcher. The other
  # two layers are things a reader might genuinely want to turn off; the
  # world is not.
  p <- tmap::tm_basemap(NULL) +
    tmap::tm_shape(world, name = "World") +
    tmap::tm_polygons(
      fill = unname(NETRUNNER_MAP_SURFACE[["map_nodata"]]),
      col = unname(NETRUNNER_MAP_SURFACE[["map_edge"]]),
      lwd = 0.4,
      group.control = "none"
    ) +
    tmap::tm_shape(joined, name = "Tournaments per million") +
    tmap::tm_polygons(
      fill = "per_million",
      # THE SCALE IS SPECIFIED, NOT INFERRED. tmap's interval defaults
      # are computed in get_scale_defaults(), which inside the running
      # app failed with "missing value where TRUE/FALSE needed" while
      # inferring perfectly in a plain session and under testServer().
      # Naming the style and the class count asks it to infer nothing,
      # and a fixed classification is better for this map anyway: the
      # bands stay put as the date filter moves, so two periods can
      # actually be compared by colour.
      fill.scale = tmap::tm_scale_intervals(style = "fixed",
                                            breaks = c(0, 1, 2, 4, 8, 16, Inf),
                                            values = NETRUNNER_MAP_RAMP),
      # SLIGHTLY TRANSPARENT, so the basemap's coastlines and graticule
      # read through the fill rather than being replaced by it. At full
      # opacity the choropleth stops being a layer over a map and becomes
      # a flat shape chart that happens to be country-shaped.
      fill_alpha = 0.82,
      # Borders in the page's own ground colour rather than tmap's
      # default grey: on a dark basemap a light hairline around every
      # country reads as a second, competing map.
      col = unname(NETRUNNER_PALETTE[["ground"]]),
      lwd = 0.4,
      # The legend is a panel on a dark page, so it is given the same
      # treatment as every other panel: the slate background, the edge,
      # and the two text colours the rest of the app uses. Left alone it
      # renders as white card with black text, which is the one thing on
      # the whole view that would still look borrowed.
      fill.legend = tmap::tm_legend(
        title = "Tournaments per million",
        title.color = unname(NETRUNNER_PALETTE[["ink_bright"]]),
        text.color = unname(NETRUNNER_PALETTE[["ink"]]),
        bg = TRUE,
        bg.color = unname(NETRUNNER_PALETTE[["panel_solid"]]),
        bg.alpha = 0.92,
        frame = TRUE,
        frame.color = unname(NETRUNNER_PALETTE[["ink_quiet"]])
      ),
      # No fill.scale override. The inner join above means every drawn
      # country HAS a value, so there is no missing category for tmap to
      # give a swatch to -- the "Missing" entry the old legend carried
      # was the 130-odd zero-filled countries, and they are simply not
      # drawn now. An explicit value.na = NULL here errored in view mode
      # ("missing value where TRUE/FALSE needed") while working in plot
      # mode, which is a good reason not to set what does not need setting.
      # tm_popup(), not the popup.vars argument: tmap 4 deprecates the
      # latter and warns on every render, which in a Shiny app means once
      # per session in the log for a purely cosmetic reason.
      popup = tmap::tm_popup(vars = c("Tournaments" = "tournaments",
                                      "Per million" = "per_million"))
    )

  if (nrow(venues)) {
    pts <- sf::st_as_sf(venues, coords = c("location_lng", "location_lat"),
                        crs = 4326)
    p <- p + tmap::tm_shape(pts, name = "Tournament Locations") +
      tmap::tm_bubbles(
        size = "count",
        # TITLED, or the legend is headed "count" -- the column name,
        # which means something to whoever wrote tournament_venues() and
        # nothing to anyone reading a map. The same mistake the layer
        # control made with "joined" and "pts", in the one place left
        # that still had a default to leak.
        size.legend = tmap::tm_legend(
          title = "Tournaments at venue",
          title.color = unname(NETRUNNER_PALETTE[["ink_bright"]]),
          text.color = unname(NETRUNNER_PALETTE[["ink"]]),
          bg = TRUE,
          bg.color = unname(NETRUNNER_PALETTE[["panel_solid"]]),
          bg.alpha = 0.92,
          frame = TRUE,
          frame.color = unname(NETRUNNER_PALETTE[["ink_quiet"]])
        ),
        # NOT the accent: see NETRUNNER_MAP_SURFACE["map_point"]. The
        # choropleth ramp ends at the accent, so amber bubbles over amber
        # countries made the two layers into one. Outlined in the ground
        # colour so overlapping bubbles stay countable rather than merging
        # into a single blob.
        fill = unname(NETRUNNER_MAP_SURFACE[["map_point"]]),
        fill_alpha = 0.75,
        col = unname(NETRUNNER_PALETTE[["ground"]]),
        lwd = 0.6,
        popup = tmap::tm_popup(vars = c("Tournaments here" = "count"))
      )
  }

  # The sea. In view mode leaflet paints its own container and this does
  # nothing -- the CSS on .leaflet-container is what a reader sees -- but
  # both come from the same constant, so a static render for a report or
  # a print gets the same sea as the app.
  p + tmap::tm_layout(bg.color = unname(NETRUNNER_MAP_SURFACE[["map_water"]]))
}
