#' Faction wins per side, per period
#'
#' The shaping behind both trend charts. One row per (side, period,
#' faction) with the raw count, that faction's share of its side in that
#' period, and its rank.
#'
#' PERIODS ARE ZERO-FILLED, so a faction that won nothing in a quarter
#' has a row saying so. Without it `geom_area` interpolates straight
#' across the gap and draws a faction as continuously present through
#' quarters it never appeared in.
#'
#' RANK IS NA WHERE THERE ARE NO WINS, which is the one place this
#' departs from the notebook. Ranking every faction every quarter gives
#' the ones on zero an ordering among themselves -- and that ordering
#' moves, so the bump chart shows lines crossing that encode nothing but
#' the tie-break. A faction that won nothing has no rank, the line
#' breaks, and the break is the information.
#'
#' Ties among factions that DID win break by the fixed faction order, so
#' two factions level on wins do not swap places between renders.
#'
#' @param tournaments Rows already filtered, with a `.date` column from
#'   with_parsed_dates().
#' @param identities The cardpool identity cards.
#' @param factions The cardpool `faction` table, or NULL.
#' @param unit Period size passed to cut(): "quarter", "month", "year".
#' @return A data frame of `side`, `period`, `faction_code`, `faction`,
#'   `wins`, `share`, `rank`.
#' @export
faction_quarterly_shares <- function(tournaments, identities, factions = NULL,
                                     unit = "quarter") {
  empty <- data.frame(
    side = character(0), period = as.Date(character(0)),
    faction_code = character(0), faction = character(0),
    wins = integer(0), share = numeric(0), rank = integer(0),
    stringsAsFactors = FALSE
  )
  if (is.null(tournaments) || !nrow(tournaments) || is.null(identities)) {
    return(empty)
  }
  if (!".date" %in% names(tournaments)) return(empty)

  d <- tournaments[!is.na(tournaments$.date), , drop = FALSE]
  if (!nrow(d)) return(empty)

  lookup <- stats::setNames(as.character(identities$faction_code),
                            as.character(identities$code))
  blank <- function(x) is.na(x) | !nzchar(as.character(x))

  own <- function(side) {
    if (is.null(factions) || !nrow(factions)) return(NULL)
    as.character(factions$code)[tolower(as.character(factions$side)) == side]
  }

  long <- do.call(rbind, lapply(
    list(list("Runner", "winner_runner_identity", own("runner")),
         list("Corp", "winner_corp_identity", own("corp"))),
    function(spec) {
      codes <- as.character(d[[spec[[2]]]])
      keep <- !blank(codes)
      fc <- unname(lookup[codes[keep]])
      per <- d$.date[keep]
      ok <- !is.na(fc)
      # The same side filter the totals apply: a Corp identity recorded
      # as the Runner winner is an upstream entry error, and a trend
      # chart is exactly where one stray point reads as a real event.
      if (!is.null(spec[[3]])) ok <- ok & fc %in% spec[[3]]
      if (!any(ok)) return(NULL)
      data.frame(side = spec[[1]], faction_code = fc[ok],
                 period = period_start(per[ok], unit),
                 stringsAsFactors = FALSE)
    }
  ))
  if (is.null(long) || !nrow(long)) return(empty)

  counts <- stats::aggregate(
    list(wins = rep(1L, nrow(long))),
    by = list(side = long$side, period = long$period,
              faction_code = long$faction_code),
    FUN = sum
  )

  # The grid every side/faction is filled across. Built from the periods
  # PRESENT rather than from a seq(), so a filter that selects two far
  # apart does not manufacture a hundred empty quarters between them.
  periods <- sort(unique(counts$period))
  grid <- do.call(rbind, lapply(unique(counts$side), function(sd) {
    fcs <- unique(counts$faction_code[counts$side == sd])
    expand.grid(side = sd, faction_code = fcs, period = periods,
                stringsAsFactors = FALSE)
  }))

  out <- merge(grid, counts, by = c("side", "faction_code", "period"),
               all.x = TRUE)
  out$wins[is.na(out$wins)] <- 0L
  out$faction <- faction_display_name(out$faction_code, factions)
  out <- disambiguate_faction_names(out)

  out <- do.call(rbind, lapply(
    split(out, list(out$side, out$period), drop = TRUE),
    function(g) {
      total <- sum(g$wins)
      g$share <- if (total > 0) g$wins / total else 0
      rank_order <- order(-g$wins, match(g$faction_code, FACTION_ORDER))
      g$rank <- NA_integer_
      won <- g$wins[rank_order] > 0
      g$rank[rank_order[won]] <- seq_len(sum(won))
      g
    }
  ))

  out <- out[order(factor(out$side, levels = c("Runner", "Corp")),
                   out$period,
                   match(out$faction_code, FACTION_ORDER)), , drop = FALSE]
  rownames(out) <- NULL
  out[c("side", "period", "faction_code", "faction", "wins", "share", "rank")]
}

#' Floor a date to the start of its period
#'
#' cut.Date() rather than lubridate::floor_date(): lubridate is already
#' an Import but this is the only thing the trend charts would need it
#' for, and cut() is in base.
#'
#' @param x A Date vector.
#' @param unit "quarter", "month" or "year".
#' @return A Date vector of period starts.
#' @keywords internal
period_start <- function(x, unit = "quarter") {
  as.Date(cut(x, breaks = unit))
}

#' Build a plot without the unknown-aesthetic warning
#'
#' `text` is not a ggplot2 aesthetic. It is how ggplotly() is told what to
#' put in a tooltip, and ggplot2 warns about it at LAYER CONSTRUCTION --
#' not at conversion, which is why suppressing it around ggplotly() does
#' nothing. Left alone it prints once per chart per render into the app
#' log, which is noise that would eventually hide something real.
#'
#' Only that one message is muffled. Any other warning a plot raises is
#' still worth hearing.
#'
#' @param expr A plot-building expression.
#' @return Whatever `expr` returns.
#' @keywords internal
without_unknown_aes_warning <- function(expr) {
  withCallingHandlers(expr, warning = function(w) {
    if (grepl("unknown aesthetic", conditionMessage(w), ignore.case = TRUE)) {
      invokeRestart("muffleWarning")
    }
  })
}

#' Faction share over time, as a filled area
#'
#' SHARE, NOT COUNT. The absolute number of tournaments per quarter is
#' dominated by how many events happened, which is a fact about the
#' calendar and the pandemic rather than about the meta. Filling to 100%
#' asks the question the view is for: of the games won that quarter, who
#' won them.
#'
#' The count is not lost -- it is the bump chart's ordering and the
#' summary line's total -- but it is not what this chart is answering.
#'
#' @param q Output of faction_quarterly_shares().
#' @return A ggplot, or NULL when ggplot2 is unavailable or there is
#'   nothing to draw.
#' @keywords internal
build_faction_area <- function(q) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(q) || !nrow(q)) return(NULL)

  q$side <- factor(q$side, levels = c("Runner", "Corp"))
  lv <- unique(q$faction_code[order(match(q$faction_code, FACTION_ORDER))])
  q$fill <- factor(q$faction_code, levels = lv)
  # `text` is not a ggplot2 aesthetic; it is how ggplotly() is told what
  # to put in a tooltip. Built here rather than left to plotly's default,
  # which would show the stacking position -- a cumulative number the
  # reader never asked about and cannot check.
  q$text <- sprintf("%s\n%s %s\n%d wins, %.1f%% of %s",
                    q$faction, format(q$period, "%b"), format(q$period, "%Y"),
                    q$wins, q$share * 100, tolower(as.character(q$side)))
  labels <- q$faction[match(lv, q$faction_code)]
  values <- unname(FACTION_COLOURS[lv])
  values[is.na(values)] <- "#999999"

  without_unknown_aes_warning(
    ggplot2::ggplot(q, ggplot2::aes(x = .data$period, y = .data$wins,
                                    fill = .data$fill, text = .data$text)) +
    ggplot2::geom_area(position = "fill", colour = NA) +
    ggplot2::facet_wrap(~side, ncol = 1) +
    ggplot2::scale_fill_manual(values = values, labels = labels, name = NULL) +
    # scales::label_percent(), not a hand-rolled function. ggplotly()
    # asks a scale for its labels separately from its breaks, and a
    # function that returns a value for every break INCLUDING the NAs
    # ggplot pads with gives back more labels than breaks -- plotly then
    # aborts with "`breaks` and `labels` have different lengths". The
    # static plot never noticed, because it only ever asks once.
    ggplot2::scale_y_continuous(labels = scales::label_percent(),
                                expand = c(0, 0)) +
    ggplot2::scale_x_date(date_labels = "%Y", expand = c(0, 0)) +
    ggplot2::labs(x = NULL, y = NULL) +
    nr_chart_theme()
  )
}

#' Faction rank over time, as a bump chart
#'
#' LINES BREAK WHERE A FACTION WON NOTHING. See
#' faction_quarterly_shares(): a faction on zero has no rank, so
#' `geom_line` leaves a gap rather than drawing it climbing through
#' positions it never held.
#'
#' @param q Output of faction_quarterly_shares().
#' @return A ggplot, or NULL when ggplot2 is unavailable or there is
#'   nothing to draw.
#' @keywords internal
build_faction_bump <- function(q) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)
  if (is.null(q) || !nrow(q)) return(NULL)
  ranked <- q[!is.na(q$rank), , drop = FALSE]
  if (!nrow(ranked)) return(NULL)

  ranked$side <- factor(ranked$side, levels = c("Runner", "Corp"))
  lv <- unique(ranked$faction_code[order(match(ranked$faction_code, FACTION_ORDER))])
  ranked$col <- factor(ranked$faction_code, levels = lv)
  ranked$text <- sprintf("%s\n%s %s\nrank %d of that quarter, %d wins",
                         ranked$faction, format(ranked$period, "%b"),
                         format(ranked$period, "%Y"), ranked$rank, ranked$wins)
  labels <- ranked$faction[match(lv, ranked$faction_code)]
  values <- unname(FACTION_COLOURS[lv])
  values[is.na(values)] <- "#999999"

  without_unknown_aes_warning(
    ggplot2::ggplot(ranked, ggplot2::aes(x = .data$period, y = .data$rank,
                                         colour = .data$col,
                                         group = .data$faction_code)) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::geom_point(ggplot2::aes(text = .data$text), size = 1.7) +
    ggplot2::facet_wrap(~side, ncol = 1, scales = "free_y") +
    ggplot2::scale_colour_manual(values = values, labels = labels, name = NULL) +
    ggplot2::scale_x_date(date_labels = "%Y") +
    ggplot2::scale_y_reverse(breaks = seq_len(max(ranked$rank))) +
    ggplot2::labs(x = NULL, y = NULL) +
    nr_chart_theme()
  )
}

#' The chart theme both trend plots share
#'
#' One definition, because two charts sitting one above the other with
#' different gridline colours look like a mistake rather than a pair.
#' Transparent throughout for the reason build_faction_waffle() was: a
#' white panel on this page is an image pasted onto it.
#' @keywords internal
nr_chart_theme <- function() {
  ink <- unname(NETRUNNER_PALETTE[["ink"]])
  quiet <- unname(NETRUNNER_PALETTE[["ink_quiet"]])
  ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.background = ggplot2::element_rect(fill = "transparent", colour = NA),
      legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
      panel.grid.major = ggplot2::element_line(colour = "#1e2630", linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(colour = quiet, size = 9),
      strip.text = ggplot2::element_text(
        colour = unname(NETRUNNER_PALETTE[["ink_bright"]]),
        face = "bold", size = 12, hjust = 0,
        margin = ggplot2::margin(t = 4, b = 4)
      ),
      legend.position = "bottom",
      legend.text = ggplot2::element_text(colour = ink, size = 9),
      legend.key.size = grid::unit(0.45, "cm"),
      plot.margin = ggplot2::margin(4, 8, 4, 4)
    )
}

#' Winning Runner/Corp faction pairings, as a matrix
#'
#' One tournament contributes ONE cell: the faction of its winning Runner
#' identity against the faction of its winning Corp identity. That is a
#' pairing of two decks the same person brought, not a matchup between
#' two opponents -- the chord diagram shows which faction pairs people
#' win with, and nothing about which beats which.
#'
#' Rows are Corp and columns Runner, which is what chorddiag's bipartite
#' mode expects.
#'
#' @param tournaments Rows already filtered.
#' @param identities The cardpool identity cards.
#' @param factions The cardpool `faction` table. Required: the sides come
#'   from it, and without it there is no way to know which factions
#'   belong on which axis.
#' @return A list of `matrix`, `corp` and `runner` (the faction display
#'   names in axis order), or NULL when there is nothing to draw.
#' @export
faction_pairing_matrix <- function(tournaments, identities, factions) {
  if (is.null(tournaments) || !nrow(tournaments) || is.null(identities) ||
      is.null(factions) || !nrow(factions)) {
    return(NULL)
  }

  lookup <- stats::setNames(as.character(identities$faction_code),
                            as.character(identities$code))
  rf <- unname(lookup[as.character(tournaments$winner_runner_identity)])
  cf <- unname(lookup[as.character(tournaments$winner_corp_identity)])

  runner_codes <- as.character(factions$code)[
    tolower(as.character(factions$side)) == "runner"]
  corp_codes <- as.character(factions$code)[
    tolower(as.character(factions$side)) == "corp"]

  # BOTH ends must be present and on the right side. A tournament missing
  # either winner is not a pairing, and one with a Corp identity in the
  # Runner column is the upstream entry error the totals already drop.
  ok <- !is.na(rf) & !is.na(cf) & rf %in% runner_codes & cf %in% corp_codes
  if (!any(ok)) return(NULL)
  rf <- rf[ok]; cf <- cf[ok]

  # Only factions that actually appear: an axis of empty spokes is a
  # ring of labels attached to nothing.
  runner_codes <- runner_codes[runner_codes %in% rf]
  corp_codes <- corp_codes[corp_codes %in% cf]
  runner_codes <- runner_codes[order(match(runner_codes, FACTION_ORDER))]
  corp_codes <- corp_codes[order(match(corp_codes, FACTION_ORDER))]

  m <- matrix(0L, nrow = length(corp_codes), ncol = length(runner_codes))
  tab <- table(factor(cf, levels = corp_codes), factor(rf, levels = runner_codes))
  m[] <- as.integer(tab)

  runner_names <- faction_display_name(runner_codes, factions)
  corp_names <- faction_display_name(corp_codes, factions)
  # The two Neutrals are spelled the same upstream, and a chord ring with
  # two identical labels is unreadable.
  if (any(runner_names %in% corp_names)) {
    dupe <- intersect(runner_names, corp_names)
    runner_names[runner_names %in% dupe] <-
      paste0(runner_names[runner_names %in% dupe], " (R)")
    corp_names[corp_names %in% dupe] <-
      paste0(corp_names[corp_names %in% dupe], " (C)")
  }
  dimnames(m) <- list(Corp = corp_names, Runner = runner_names)

  list(matrix = m, corp = corp_codes, runner = runner_codes,
       corp_names = corp_names, runner_names = runner_names)
}

#' Hand a ggplot to plotly, themed for this page
#'
#' INTERACTIVE, because a share and a rank are both numbers a reader
#' wants to read off rather than estimate against an axis. The tooltip
#' says what the point IS -- faction, quarter, wins, share -- rather than
#' plotly's default, which for a filled area reports the stacking
#' position: a cumulative number nobody asked for and cannot check.
#'
#' PLOTLY RATHER THAN ggiraph, and the reason is the bug this page
#' already has. ggiraph ships its own d3, and d3treeR's treemap on the
#' same page needs d3 v3; htmlwidgets deduplicates dependencies by name
#' and keeps one, so the loser fails silently. plotly declares no d3
#' dependency at all -- `plotly-main` is a self-contained bundle -- so it
#' cannot join that argument.
#'
#' @param p A ggplot, or NULL.
#' @return A plotly htmlwidget, or NULL.
#' @keywords internal
nr_plotly <- function(p) {
  if (is.null(p) || !requireNamespace("plotly", quietly = TRUE)) return(NULL)
  ink <- unname(NETRUNNER_PALETTE[["ink"]])

  w <- plotly::ggplotly(p, tooltip = "text")

  w <- plotly::layout(
    w,
    paper_bgcolor = "transparent", plot_bgcolor = "transparent",
    font = list(color = ink, size = 12),
    hoverlabel = list(
      bgcolor = unname(NETRUNNER_PALETTE[["panel_solid"]]),
      bordercolor = unname(NETRUNNER_PALETTE[["accent"]]),
      font = list(color = ink)
    ),
    legend = list(orientation = "h", y = -0.12, font = list(color = ink)),
    margin = list(l = 40, r = 10, t = 30, b = 40)
  )

  # The modebar is a row of plotly's own icons over the corner of the
  # chart. Hover and legend clicks are the interaction worth having here;
  # zoom, lasso select and "download as png" are not.
  plotly::config(w, displayModeBar = FALSE, displaylogo = FALSE)
}
