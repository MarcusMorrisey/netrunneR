#' Tournament counts per country, ready to join to a world map
#'
#' Shapes the abr `tournament` table into the frame the map draws: one
#' row per country, a tournament count, and that count per million
#' population once joined to geometry.
#'
#' COUNTRY NAMES ARE JOINED, NOT COORDINATES, so the join key is a string
#' and strings disagree. `World` (tmap) spells the United States "United
#' States of America" where abr says "United States"; without that one
#' rename the country with the most tournaments in the data -- 1,111 of
#' them, a quarter of the whole set -- silently vanishes from the map.
#' That is the failure mode this function exists to prevent: a map that
#' renders beautifully and is missing its largest value.
#'
#' Renames are applied from a table rather than a chain of if_else()s so
#' that an unmatched country is REPORTABLE. `unmatched` comes back in the
#' result rather than being dropped quietly, because a country that fails
#' to join looks exactly like a country with no tournaments once it is
#' drawn, and only one of those is true.
#'
#' Rows with no country at all are dropped -- 520 of them in the current
#' data. They are not a country that failed to match; abr simply has no
#' location for them, and there is nowhere on a map to put them.
#'
#' @param tournaments The abr `tournament` table.
#' @param map_names Character vector of country names as the map spells
#'   them, used to report which of ours fail to match. NULL skips the
#'   check rather than inventing an answer.
#' @return A list with `counts` (a data frame of `name`, `tournaments`,
#'   `players`) and `unmatched` (country names present in the data that
#'   the map has no polygon for).
#' @export
tournament_country_counts <- function(tournaments, map_names = NULL) {
  d <- tournaments[!is.na(tournaments$location_country) &
                     nzchar(tournaments$location_country), , drop = FALSE]

  d$name <- country_map_name(d$location_country)

  counts <- stats::aggregate(
    cbind(tournaments = rep(1L, nrow(d)),
          players = ifelse(is.na(d$players_count), 0L, d$players_count)) ~ name,
    data = d, FUN = sum
  )
  counts <- counts[order(-counts$tournaments), , drop = FALSE]

  unmatched <- if (is.null(map_names)) {
    character(0)
  } else {
    sort(setdiff(counts$name, map_names))
  }

  list(counts = counts, unmatched = unmatched)
}

#' Spell a country the way the world map does
#'
#' ONE ENTRY, because measuring found one difference. Comparing all 46
#' country names abr records against the 177 in tmap's `World`, with no
#' renaming at all, leaves five unmatched: the United States, and then
#' Hong Kong, Malta, Singapore and Vatican City.
#'
#' Only the first is a naming difference. The other four have no polygon
#' in a 177-country world map at all -- they are small states and
#' territories the dataset does not carry -- so no rename can place them
#' and inventing one would only move the failure somewhere harder to see.
#' They surface through tournament_country_counts()'s `unmatched`
#' instead, and the view names them.
#'
#' AN EARLIER VERSION OF THIS TABLE GUESSED, and the guesses were worse
#' than useless: it mapped Czechia to "Czech Rep." and South Korea to
#' "Korea", neither of which the map uses. Both countries match perfectly
#' when left alone, so the renames BROKE two countries that already
#' worked. That is why the unmatched set is reported rather than dropped
#' -- it is what caught them.
#'
#' @param x Character vector of country names as abr spells them.
#' @return `x`, respelled where a difference is known.
#' @keywords internal
country_map_name <- function(x) {
  renames <- c("United States" = "United States of America")
  hit <- match(x, names(renames))
  ifelse(is.na(hit), x, unname(renames)[hit])
}

#' Distinct venue locations, for the point layer
#'
#' Coordinates are rounded and grouped, so the point layer is venue
#' density rather than a pin per tournament -- a shop that hosts thirty
#' events is one bubble sized thirty, not thirty bubbles on top of each
#' other.
#'
#' RETURNS ZERO ROWS RATHER THAN FAILING when the release predates the
#' coordinate columns. Those columns were added to the abr allowlist
#' after this store had already been built, and a release is promoted
#' independently of the package that reads it, so a caller must be able
#' to ask for venues from a release that has none. The map then draws its
#' country layer alone, which is a weaker map and not a broken one.
#'
#' @param tournaments The abr `tournament` table.
#' @param digits Integer. Decimal places to round coordinates to before
#'   grouping. 1 is roughly 11 km, which merges venues within a city
#'   without merging cities.
#' @return A data frame of `location_lng`, `location_lat`, `count`.
#' @export
tournament_venues <- function(tournaments, digits = 1L) {
  empty <- data.frame(location_lng = numeric(0), location_lat = numeric(0),
                      count = integer(0))
  if (!all(c("location_lat", "location_lng") %in% names(tournaments))) return(empty)

  d <- tournaments[!is.na(tournaments$location_lat) &
                     !is.na(tournaments$location_lng), , drop = FALSE]
  if (nrow(d) == 0) return(empty)

  d$location_lat <- round(as.numeric(d$location_lat), digits)
  d$location_lng <- round(as.numeric(d$location_lng), digits)

  out <- stats::aggregate(
    list(count = rep(1L, nrow(d))),
    by = list(location_lng = d$location_lng, location_lat = d$location_lat),
    FUN = sum
  )
  out[order(-out$count), , drop = FALSE]
}

#' Parse an abr tournament date
#'
#' abr writes dates as "2026.08.27." -- dot-separated, with a TRAILING
#' dot. as.Date() returns NA for that format, silently, for every row, so
#' a filter built on it would quietly match nothing and look like a data
#' problem rather than a parsing one.
#'
#' THE FORMAT IS STATED, not guessed. as.Date() with no format tries a
#' short list of layouts and THROWS on anything that matches none of them
#' -- "character string is not in a standard unambiguous format" -- which
#' is the opposite of what the paragraph above promises. One malformed
#' date anywhere in a release would have aborted the whole view rather
#' than dropping a row. Naming the format makes the failure an NA, which
#' every caller already handles, and removes the guessing besides.
#'
#' @param x Character vector of abr dates.
#' @return A Date vector, NA where unparseable.
#' @keywords internal
parse_abr_date <- function(x) {
  cleaned <- sub("[.]$", "", as.character(x))
  cleaned <- gsub("[.]", "-", cleaned)
  suppressWarnings(as.Date(cleaned, format = "%Y-%m-%d"))
}

#' The rotation periods, as ranges a date filter can use
#'
#' A rotation is recorded as a single START date, so the PERIOD it names
#' runs from that date to the day before the next one -- and for the most
#' recent, to whatever the end of the data is. Derived from the table
#' rather than hardcoded, so an eighth rotation appears here by being
#' synced rather than by someone remembering to edit a list.
#'
#' The span before the first rotation is included and named, because a
#' third of this data predates 2017 and a filter that cannot reach it
#' would hide it.
#'
#' @param rotation The cardpool `rotation` table, or NULL.
#' @param max_date Date. The end of the data, for the open-ended last
#'   period.
#' @return A data frame of `label`, `start`, `end`, newest first. Zero
#'   rows when there is no rotation table, rather than invented periods.
#' @export
rotation_periods <- function(rotation, max_date = Sys.Date()) {
  empty <- data.frame(label = character(0), start = as.Date(character(0)),
                      end = as.Date(character(0)), stringsAsFactors = FALSE)
  if (is.null(rotation) || !nrow(rotation)) return(empty)

  r <- rotation[order(rotation$date_start), , drop = FALSE]
  starts <- as.Date(r$date_start)
  ends <- c(starts[-1] - 1, max_date)

  out <- data.frame(
    label = as.character(r$name), start = starts, end = ends,
    stringsAsFactors = FALSE
  )
  pre <- data.frame(
    label = "Before first rotation",
    start = as.Date("2012-01-01"), end = starts[[1]] - 1,
    stringsAsFactors = FALSE
  )
  out <- rbind(pre, out)
  out[order(out$start, decreasing = TRUE), , drop = FALSE]
}

#' The identities that mean "this was a draft"
#'
#' MEASURED, NOT LISTED BY NAME. The cardpool holds exactly seven
#' identities in the two neutral factions, and all seven are draft or
#' limited-format identities: The Masque, The Catalyst and Nova Initiumia
#' on the Runner side; The Shadow, The Syndicate, Cyber Bureau and
#' Ampere on the Corp. There is no neutral identity that is NOT one of
#' these, so the faction code is a complete and exact test -- naming the
#' seven would be the same set, spelled in a way that goes stale.
#'
#' They win 153 of the 4,431 tournaments in the current release, 147 of
#' those on both sides at once, which is what a draft event looks like.
#' The interesting residue is the 67 filed under `standard`: a draft
#' identity cannot legally win a standard event, so those are
#' mis-recorded rather than surprising.
#'
#' @param identities The cardpool identity cards.
#' @return Character vector of card codes.
#' @export
draft_identity_codes <- function(identities) {
  if (is.null(identities) || !nrow(identities)) return(character(0))
  as.character(identities$code)[
    identities$faction_code %in% c("neutral-runner", "neutral-corp")
  ]
}

#' The tournament types present in the data, most common first
#'
#' Read from the data rather than hardcoded, so abr adding an eighteenth
#' type makes it appear here by being synced.
#'
#' RETURNS NOTHING WHEN THE COLUMN IS ABSENT, rather than failing. `type`
#' was admitted to the abr allowlist after this store had already been
#' built, and a release is promoted independently of the package that
#' reads it -- the same situation the venue coordinates were in. A caller
#' must be able to ask a release that predates the column.
#'
#' One upstream record carries abr's own dropdown SEPARATOR as its type
#' rather than a real value. It is dropped here: it is not a kind of
#' tournament, it is a horizontal rule that escaped into the data.
#'
#' @param tournaments The abr `tournament` table.
#' @return A data frame of `type` and `n`, or zero rows.
#' @export
tournament_types <- function(tournaments) {
  empty <- data.frame(type = character(0), n = integer(0),
                      stringsAsFactors = FALSE)
  if (is.null(tournaments) || !nrow(tournaments)) return(empty)
  if (!"type" %in% names(tournaments)) return(empty)

  v <- as.character(tournaments$type)
  v <- v[!is.na(v) & nzchar(v)]
  # abr's dropdown separator, which is drawn with box characters and is
  # not a type of anything.
  v <- v[!grepl("^[\u2500-\u257f[:space:]]*$", v)]
  v <- v[!grepl("^[\u2500-\u257f]", v)]
  if (!length(v)) return(empty)

  tab <- sort(table(v), decreasing = TRUE)
  data.frame(type = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
}
