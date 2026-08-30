#' Faction colours, keyed by faction code
#'
#' The palette from the analysis notebook, which took it from the
#' community's RGB faction colours. Converted to hex once here rather
#' than recomputed from rgb() at draw time.
#'
#' KEYED BY CODE, NOT BY DISPLAY NAME. The notebook keyed on title-case
#' names and paid for it with a chain of repairs -- str_to_title() then
#' an if_else() to turn "Nbn" back into "NBN", then another to name the
#' NAs. Codes are what the data actually carries and what the join uses,
#' so nothing has to be respelled to look a colour up. The display name
#' comes from the cardpool `faction` table, which is also where "Sunny
#' Lebeau" is spelled the way the game spells it.
#'
#' `mini-faction` is absent because it is not a faction. The notebook
#' invented it to pool Adam, Apex and Sunny; they are three separate
#' factions in the data and are drawn as three.
#'
#' @keywords internal
FACTION_COLOURS <- c(
  anarch               = "#E86940",
  criminal             = "#547DB8",
  shaper               = "#38B359",
  adam                 = "#B3A347",
  apex                 = "#CC695E",
  `sunny-lebeau`       = "#8F8F8A",
  # LIGHTENED FROM THE NOTEBOOK'S rgb(.2, .2, .2). That is #333333, which
  # is very nearly the app's black ground -- on a chart with a
  # transparent background a Neutral square simply is not there, and 3.5%
  # of Runner wins read as a hole in the waffle. The notebook drew on
  # white, where it was fine.
  `neutral-runner`     = "#6E6E6E",
  `haas-bioroid`       = "#705494",
  jinteki              = "#EB4A57",
  nbn                  = "#FAD14F",
  `weyland-consortium` = "#4A8A78",
  `neutral-corp`       = "#CCCCCC"
)

#' The order factions are drawn in
#'
#' Runner factions then Corp, each in the order the game itself lists
#' them, which is the order the notebook used. Not alphabetical and not
#' by size: a chart whose categories reorder themselves as the date
#' filter moves cannot be compared across two periods, which is most of
#' what a date filter is for.
#' @keywords internal
FACTION_ORDER <- names(FACTION_COLOURS)

#' Faction wins per side, from tournament results
#'
#' abr records the winning Runner and Corp identity of each tournament as
#' card codes; the faction is reached by joining those to the cardpool
#' `card` table. Nothing is inferred from the code string.
#'
#' SHARES ARE WITHIN SIDE, NOT ACROSS BOTH. Every concluded tournament
#' records one Runner winner and one Corp winner -- they are the two
#' decks of the same person -- so a Runner-versus-Corp share is exactly
#' 50/50 by construction and always will be. Measured on the current
#' release: 4,203 of each, to the row. The notebook's cross-side
#' percentage was a fact about the format rather than about the meta, and
#' it is not computed here.
#'
#' TOURNAMENTS WITH NO RECORDED WINNER ARE COUNTED, NOT DROPPED SILENTLY.
#' 228 of them in the current release. They come back as `undecided` so a
#' view can say how much of the data is not in the chart, because a
#' tournament nobody reported and a tournament that did not happen look
#' identical once drawn.
#'
#' A WINNER ON THE WRONG SIDE IS DROPPED, and counted as `misfiled`. Five
#' rows in the current release name a Corp identity as the Runner winner
#' or the reverse -- one Weyland-Consortium and two Neutral-Corp
#' identities in the Runner column, one Criminal and one Neutral-Runner
#' in the Corp column. There is no reading of the game in which those are
#' true; they are upstream entry errors. Left in, each one puts a faction
#' in a chart it cannot appear in -- and it also puts TWO factions named
#' "Neutral" in the same legend, because each side has one.
#'
#' This needs `factions`, which is the table that knows a faction's side.
#' Without it nothing is dropped, because a filter that cannot be checked
#' is a guess.
#'
#' @param tournaments The abr `tournament` table.
#' @param identities The cardpool identity cards. Must carry `code` and
#'   `faction_code`. A winner is matched by JOINING on the code, so
#'   handing this the app's ice/breaker pool matches nothing and returns
#'   zero rows -- wrong-looking rather than wrong, which is the safer of
#'   the two failures but still worth not arranging.
#' @param factions The cardpool `faction` table (`code`, `name`, `side`),
#'   or NULL to fall back to the codes as display names.
#' @return A list with `wins` (a data frame of `side`, `faction_code`,
#'   `faction`, `wins`, `share`), `undecided` (integer count of
#'   tournaments with no winner recorded on either side) and `misfiled`
#'   (integer count of wins credited to a faction from the other side).
#' @export
tournament_faction_wins <- function(tournaments, identities, factions = NULL) {
  empty <- data.frame(
    side = character(0), faction_code = character(0), faction = character(0),
    wins = integer(0), share = numeric(0), stringsAsFactors = FALSE
  )
  if (is.null(tournaments) || !nrow(tournaments) || is.null(identities)) {
    return(list(wins = empty, undecided = 0L, misfiled = 0L))
  }

  blank <- function(x) is.na(x) | !nzchar(as.character(x))
  undecided <- sum(blank(tournaments$winner_runner_identity) &
                     blank(tournaments$winner_corp_identity))

  lookup <- stats::setNames(as.character(identities$faction_code),
                            as.character(identities$code))

  # Which factions belong to which side, when the table that knows is on
  # hand. NULL means no filtering rather than a guess at it.
  side_factions <- function(side) {
    if (is.null(factions) || !nrow(factions)) return(NULL)
    as.character(factions$code)[tolower(as.character(factions$side)) == side]
  }

  misfiled <- 0L
  side_wins <- function(codes, side, own) {
    codes <- as.character(codes)
    fc <- unname(lookup[codes[!blank(codes)]])
    fc <- fc[!is.na(fc)]
    if (!is.null(own)) {
      # KNOWN AND ON THE OTHER SIDE. An earlier version dropped anything
      # not in `own`, which also swept up any faction the cardpool's
      # `faction` table has never heard of -- a genuinely new faction
      # would have vanished from the chart without a word, filed under
      # "wrong side" when the truth is "not in the lookup yet". Unknown
      # codes stay in and surface through the view's unknown-colour note,
      # which is the honest place for them.
      known <- as.character(factions$code)
      wrong <- fc %in% known & !fc %in% own
      misfiled <<- misfiled + sum(wrong)
      fc <- fc[!wrong]
    }
    if (!length(fc)) return(empty)
    tab <- table(fc)
    data.frame(
      side = side, faction_code = names(tab), faction = names(tab),
      wins = as.integer(tab), share = as.numeric(tab) / sum(tab),
      stringsAsFactors = FALSE
    )
  }

  out <- rbind(
    side_wins(tournaments$winner_runner_identity, "Runner", side_factions("runner")),
    side_wins(tournaments$winner_corp_identity, "Corp", side_factions("corp"))
  )
  if (!nrow(out)) {
    return(list(wins = empty, undecided = as.integer(undecided),
                misfiled = as.integer(misfiled)))
  }

  out$faction <- faction_display_name(out$faction_code, factions)

  # THE TWO NEUTRALS ARE DISAMBIGUATED. The cardpool spells both
  # `neutral-runner` and `neutral-corp` "Neutral", which is unambiguous
  # in a table with a side column and not in a shared legend -- the
  # waffle facets by side but draws one legend for both, so it listed
  # "Neutral" twice in two different colours with nothing to tell them
  # apart. The side is appended only where a name is actually used more
  # than once, so nothing else grows a suffix it does not need.
  dupes <- out$faction[duplicated(out$faction)]
  needs <- out$faction %in% dupes
  out$faction[needs] <- sprintf("%s (%s)", out$faction[needs], out$side[needs])

  # Ordered by the fixed list, so the legend and the colours stay put as
  # the date filter moves. A code the list does not know goes last, in
  # the order it appeared, rather than being dropped -- an unknown
  # faction is a thing to notice, not to hide.
  rank <- match(out$faction_code, FACTION_ORDER)
  rank[is.na(rank)] <- length(FACTION_ORDER) + seq_len(sum(is.na(rank)))
  out <- out[order(factor(out$side, levels = c("Runner", "Corp")), rank), ,
             drop = FALSE]
  rownames(out) <- NULL

  list(wins = out[c("side", "faction_code", "faction", "wins", "share")],
       undecided = as.integer(undecided), misfiled = as.integer(misfiled))
}

#' Spell a faction the way the cardpool does
#'
#' @param codes Character vector of faction codes.
#' @param factions The cardpool `faction` table, or NULL.
#' @return Display names, falling back to the code itself when unknown --
#'   a visible raw code beats a blank label, which reads as missing data.
#' @keywords internal
faction_display_name <- function(codes, factions = NULL) {
  if (is.null(factions) || !nrow(factions)) return(codes)
  hit <- match(codes, as.character(factions$code))
  ifelse(is.na(hit), codes, as.character(factions$name)[hit])
}

#' Fill a fixed number of boxes proportionally, losing nothing
#'
#' LARGEST REMAINDER, not rounding. Rounding each share independently
#' gives a total of 98 or 103 boxes rather than 100, and a waffle whose
#' squares do not add up to a square is worse than no waffle. This floors
#' every share, then hands the leftover boxes to the largest fractional
#' parts, so the total is exactly `total` for any input.
#'
#' Ties break by position, which is faction order, so the allocation is
#' deterministic -- the same data always draws the same chart.
#'
#' @param shares Numeric vector summing to 1.
#' @param total Integer. Boxes to distribute.
#' @return Integer vector the same length as `shares`, summing to `total`.
#' @keywords internal
largest_remainder <- function(shares, total = 100L) {
  if (!length(shares)) return(integer(0))
  exact <- shares * total
  boxes <- floor(exact)
  short <- total - sum(boxes)
  if (short > 0) {
    take <- order(exact - boxes, decreasing = TRUE)[seq_len(short)]
    boxes[take] <- boxes[take] + 1
  }
  as.integer(boxes)
}

#' One row per waffle square
#'
#' ONE HUNDRED SQUARES PER SIDE, faceted, rather than one hundred shared
#' between them. See tournament_faction_wins(): the Runner/Corp split is
#' 50/50 by construction, so a shared grid spends half its squares
#' restating the rules of the game. Per side, one square is one percent
#' of that side's wins, which is the question the chart is actually for.
#'
#' @param wins The `wins` frame from tournament_faction_wins().
#' @param n_rows Integer. Squares per column.
#' @param total Integer. Squares per side.
#' @return A data frame of `side`, `faction_code`, `faction`, `row`,
#'   `column`, one row per square.
#' @export
faction_waffle_squares <- function(wins, n_rows = 5L, total = 100L) {
  empty <- data.frame(side = character(0), faction_code = character(0),
                      faction = character(0), row = integer(0),
                      column = integer(0), stringsAsFactors = FALSE)
  if (is.null(wins) || !nrow(wins)) return(empty)

  per_side <- lapply(
    split(wins, factor(wins$side, levels = c("Runner", "Corp"))),
    function(d) {
      if (!nrow(d)) return(NULL)
      d$boxes <- largest_remainder(d$wins / sum(d$wins), total)
      d <- d[d$boxes > 0, , drop = FALSE]
      if (!nrow(d)) return(NULL)
      cells <- d[rep(seq_len(nrow(d)), d$boxes), , drop = FALSE]
      i <- seq_len(nrow(cells)) - 1L
      data.frame(
        side = cells$side, faction_code = cells$faction_code,
        faction = cells$faction,
        row = n_rows - i %% n_rows,
        column = i %/% n_rows + 1L,
        stringsAsFactors = FALSE
      )
    }
  )

  out <- do.call(rbind, per_side[!vapply(per_side, is.null, logical(1))])
  if (is.null(out)) return(empty)
  rownames(out) <- NULL
  out
}

#' Factions with a win but not enough for one square
#'
#' At one square per percent, a faction under half a percent of its
#' side rounds to nothing -- Apex, with 19 wins, is 0.45% of the Runner
#' side and draws no square at all.
#'
#' They are NOT given a square anyway. A hundred squares meaning a
#' hundred percent is the whole contract of the chart, and topping up the
#' small factions breaks it silently. They are named underneath instead,
#' which says the same thing without lying about the arithmetic.
#'
#' @param wins The `wins` frame from tournament_faction_wins().
#' @param n_rows,total As faction_waffle_squares().
#' @return The rows of `wins` that draw no square.
#' @keywords internal
factions_below_resolution <- function(wins, n_rows = 5L, total = 100L) {
  if (is.null(wins) || !nrow(wins)) return(wins)
  drawn <- faction_waffle_squares(wins, n_rows = n_rows, total = total)
  wins[!wins$faction_code %in% drawn$faction_code, , drop = FALSE]
}

#' The faction waffle
#'
#' @param wins The `wins` frame from tournament_faction_wins().
#' @param n_rows Integer. Squares per column.
#' @return A ggplot, or NULL when ggplot2 is unavailable or there is
#'   nothing to draw.
#' @keywords internal
build_faction_waffle <- function(wins, n_rows = 5L) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  squares <- faction_waffle_squares(wins, n_rows = n_rows)
  if (!nrow(squares)) return(NULL)

  # Levels come from what was actually DRAWN, in fixed order. Taking them
  # from `wins` instead put a legend entry with no swatch next to it for
  # any faction whose share rounded to zero squares -- ggplot2 draws no
  # key glyph for a level with no rows, so the reader got a floating
  # label attached to nothing. Those factions are named under the chart
  # by factions_below_resolution() instead.
  #
  # Fixed order regardless: never resorted by size, so the legend does
  # not reshuffle as the date filter moves.
  lv <- intersect(unique(wins$faction_code), unique(squares$faction_code))
  squares$fill <- factor(squares$faction_code, levels = lv)
  squares$side <- factor(squares$side, levels = c("Runner", "Corp"))
  labels <- wins$faction[match(lv, wins$faction_code)]
  values <- unname(FACTION_COLOURS[lv])
  values[is.na(values)] <- "#999999"

  ggplot2::ggplot(squares, ggplot2::aes(x = .data$column, y = .data$row,
                                        fill = .data$fill)) +
    ggplot2::geom_tile(width = 0.86, height = 0.86,
                       colour = unname(NETRUNNER_PALETTE[["ground"]]),
                       linewidth = 0.3) +
    ggplot2::facet_wrap(~side, ncol = 1) +
    ggplot2::scale_fill_manual(values = values, labels = labels, name = NULL,
                               drop = FALSE) +
    ggplot2::scale_x_continuous(breaks = NULL, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2, byrow = TRUE)) +
    ggplot2::coord_equal() +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      # TRANSPARENT, NOT WHITE. renderPlot() draws on white by default,
      # which on this app's black ground puts a bright rectangle around
      # the chart -- the plot stops being part of the page and starts
      # being an image pasted onto it. Transparent here AND bg =
      # "transparent" at the renderPlot() call; either one alone still
      # leaves a white panel behind the squares.
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      strip.text = ggplot2::element_text(size = 13, face = "bold", hjust = 0,
                                         colour = unname(NETRUNNER_PALETTE[["ink_bright"]]),
                                         margin = ggplot2::margin(t = 6, b = 4)),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(
        size = 9, colour = unname(NETRUNNER_PALETTE[["ink"]])
      ),
      legend.key.size = grid::unit(0.5, "cm"),
      plot.margin = ggplot2::margin(4, 4, 4, 4)
    )
}

#' The nested structure the interactive treemap draws
#'
#' Two branches, Runner and Corp, each holding its factions. The two
#' branches are equal in area BY CONSTRUCTION and not by measurement --
#' see tournament_faction_wins(). They are kept because the drill-down is
#' the point: the proportions inside each branch are what vary, and the
#' view says so above the chart rather than leaving a reader to conclude
#' from equal halves that the sides are evenly matched.
#'
#' Percentages in the labels are within side, matching the waffle, so the
#' two charts on the page cannot quote different numbers for the same
#' faction.
#'
#' @param wins The `wins` frame from tournament_faction_wins().
#' @return A nested list suitable for `d3treeR::d3tree2()`, or NULL when
#'   there is nothing to draw.
#' @keywords internal
faction_treemap_hierarchy <- function(wins) {
  if (is.null(wins) || !nrow(wins)) return(NULL)
  sides <- split(wins, factor(wins$side, levels = c("Runner", "Corp")))
  sides <- sides[vapply(sides, nrow, integer(1)) > 0]
  if (!length(sides)) return(NULL)

  list(
    name = "Tournament wins",
    children = unname(lapply(sides, function(d) {
      list(
        name = as.character(d$side[[1]]),
        color = if (identical(as.character(d$side[[1]]), "Runner")) "#2B6CB0" else "#C53030",
        children = unname(lapply(seq_len(nrow(d)), function(i) {
          colour <- unname(FACTION_COLOURS[d$faction_code[[i]]])
          list(
            name = sprintf("%s (%.1f%%)", d$faction[[i]], d$share[[i]] * 100),
            size = d$wins[[i]],
            color = if (is.na(colour)) "#999999" else colour
          )
        }))
      )
    }))
  )
}
