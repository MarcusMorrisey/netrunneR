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

  out <- disambiguate_faction_names(out)

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

#' Append the side where two factions share a display name
#'
#' The cardpool spells both `neutral-runner` and `neutral-corp`
#' "Neutral", which is unambiguous in a table with a side column and not
#' in a shared legend -- every chart here facets by side and draws ONE
#' legend for both, so it listed "Neutral" twice in two different colours
#' with nothing to tell them apart.
#'
#' The side is appended only where a name is genuinely used more than
#' once, so nothing else grows a suffix it does not need.
#'
#' @param d A data frame with `side` and `faction` columns.
#' @return `d`, with duplicated display names made unique.
#' @keywords internal
disambiguate_faction_names <- function(d) {
  if (is.null(d) || !nrow(d)) return(d)
  pairs <- unique(d[c("side", "faction")])
  dupes <- pairs$faction[duplicated(pairs$faction)]
  needs <- d$faction %in% dupes
  d$faction[needs] <- sprintf("%s (%s)", d$faction[needs], d$side[needs])
  d
}

#' Faction wins broken down to the winning identity
#'
#' The same counting as tournament_faction_wins(), one level deeper. The
#' treemap drills side -> faction -> identity, and an identity is the
#' thing a reader actually recognises: "Anarch won a third of everything"
#' is a fact about the game, "Hoshiko won a third of Anarch's" is a fact
#' about the meta.
#'
#' Identities are NAMED, not coded. `identities` must carry `title`; a
#' release predating that column falls back to the code, which is ugly
#' and visible rather than blank and wrong.
#'
#' @param tournaments Rows already filtered.
#' @param identities The cardpool identity cards.
#' @param factions The cardpool `faction` table, or NULL.
#' @return A data frame of `side`, `faction_code`, `faction`, `identity`,
#'   `wins`, ordered by side, faction order, then wins descending.
#' @export
tournament_identity_wins <- function(tournaments, identities, factions = NULL) {
  empty <- data.frame(side = character(0), faction_code = character(0),
                      faction = character(0), identity = character(0),
                      wins = integer(0), stringsAsFactors = FALSE)
  if (is.null(tournaments) || !nrow(tournaments) || is.null(identities)) {
    return(empty)
  }

  fac <- stats::setNames(as.character(identities$faction_code),
                         as.character(identities$code))
  nm <- if ("title" %in% names(identities)) {
    stats::setNames(as.character(identities$title), as.character(identities$code))
  } else {
    stats::setNames(as.character(identities$code), as.character(identities$code))
  }
  blank <- function(x) is.na(x) | !nzchar(as.character(x))

  own <- function(side) {
    if (is.null(factions) || !nrow(factions)) return(NULL)
    as.character(factions$code)[tolower(as.character(factions$side)) == side]
  }

  one <- function(col, side, allowed) {
    codes <- as.character(tournaments[[col]])
    codes <- codes[!blank(codes)]
    fc <- unname(fac[codes])
    ok <- !is.na(fc)
    # The same side filter the totals apply, so a mis-filed winner does
    # not appear as an identity of a faction it cannot belong to.
    if (!is.null(allowed)) ok <- ok & fc %in% allowed
    if (!any(ok)) return(empty)
    tab <- table(fc[ok], unname(nm[codes[ok]]))
    idx <- which(tab > 0, arr.ind = TRUE)
    data.frame(
      side = side,
      faction_code = rownames(tab)[idx[, 1]],
      faction = rownames(tab)[idx[, 1]],
      identity = colnames(tab)[idx[, 2]],
      wins = as.integer(tab[idx]),
      stringsAsFactors = FALSE
    )
  }

  out <- rbind(one("winner_runner_identity", "Runner", own("runner")),
               one("winner_corp_identity", "Corp", own("corp")))
  if (!nrow(out)) return(empty)

  out$faction <- faction_display_name(out$faction_code, factions)
  out <- disambiguate_faction_names(out)

  rank <- match(out$faction_code, FACTION_ORDER)
  rank[is.na(rank)] <- length(FACTION_ORDER) + seq_len(sum(is.na(rank)))
  out <- out[order(factor(out$side, levels = c("Runner", "Corp")), rank,
                   -out$wins, out$identity), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' The nested structure the interactive treemap draws
#'
#' THREE LEVELS: side, then faction, then identity. The two branches are
#' equal in area BY CONSTRUCTION and not by measurement -- every
#' tournament records one Runner winner and one Corp winner, so the split
#' is a fact about the format. They are kept because the drill-down is
#' the point, and the view says so above the chart rather than leaving a
#' reader to read "evenly matched" off equal halves.
#'
#' Faction percentages are within side and identity counts are raw wins.
#' A percentage three levels deep is a share of a share of a half, which
#' is a number nobody can hold; the count is what a reader can compare.
#'
#' @param wins A data frame from tournament_identity_wins().
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
      side_total <- sum(d$wins)
      by_faction <- split(d, factor(d$faction_code, levels = unique(d$faction_code)))
      list(
        name = as.character(d$side[[1]]),
        color = if (identical(as.character(d$side[[1]]), "Runner")) "#2B6CB0" else "#C53030",
        children = unname(lapply(by_faction, function(f) {
          colour <- unname(FACTION_COLOURS[f$faction_code[[1]]])
          if (is.na(colour)) colour <- "#999999"
          list(
            name = sprintf("%s (%.1f%%)", f$faction[[1]],
                           sum(f$wins) / side_total * 100),
            color = colour,
            children = unname(lapply(seq_len(nrow(f)), function(i) list(
              name = sprintf("%s (%d)", f$identity[[i]], f$wins[[i]]),
              size = f$wins[[i]],
              color = colour
            )))
          )
        }))
      )
    }))
  )
}
