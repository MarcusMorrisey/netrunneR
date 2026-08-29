#' Rulings, FAQ entries and errata for one card
#'
#' NetrunnerDB keeps all three in a single `ruling` table and tells them
#' apart by a bracketed source marker inside the text -- `[Official FAQ]`
#' and `[UFAQ 24]` for FAQ answers, `[Uprising Release Notes]` and its
#' siblings for errata and rules changes, and a rules-team name
#' (`[NSG Rules Team Update]`, `[Michael Boggs]`) for everything else.
#' They are not separate feeds and this does not pretend otherwise: the
#' marker is surfaced as a label so a reader can tell an official FAQ
#' answer from one person's ruling, which is a real difference in weight.
#'
#' Keyed by TITLE, which is how NetrunnerDB publishes them. Every one of
#' the 869 titles carrying rulings matches a cardpool title exactly, so
#' nothing is lost to fuzzy matching -- and nothing is fuzzily matched.
#'
#' REVIEWS ARE NOT INCLUDED. The same lineage mirrors 4,265 user reviews;
#' those are opinion about whether a card is good, not rules about how it
#' works, and mixing them into a rulings panel would misrepresent both.
#'
#' @param rulings The `ruling` table from the active nrdb release, or NULL.
#' @param title Character. The card's title.
#' @return A data frame of that card's rulings, newest first, empty if
#'   there are none.
#' @keywords internal
rulings_for_card <- function(rulings, title) {
  # A NULL table and an empty one mean the same thing to every caller, so
  # both return the same empty frame -- NULL[0, ] is NULL, and handing a
  # caller NULL where it expects rows moves the check downstream instead
  # of settling it here.
  if (is.null(rulings)) return(empty_rulings())
  if (nrow(rulings) == 0) return(rulings)
  hit <- rulings[!is.na(rulings$title) & rulings$title == title, , drop = FALSE]
  if (nrow(hit) == 0) return(hit)
  hit[order(hit$date_update, decreasing = TRUE), , drop = FALSE]
}

#' The source marker a ruling carries, e.g. "Official FAQ"
#'
#' Returns the LAST bracketed marker, because NetrunnerDB writes the
#' attribution at the end and the body may contain bracketed Markdown
#' link text earlier -- taking the first would report a card name as the
#' source.
#' @param text Character vector of ruling bodies.
#' @return Character vector of markers, `NA` where there is none.
#' @keywords internal
ruling_source_label <- function(text) {
  vapply(text, function(x) {
    if (is.na(x)) return(NA_character_)
    # Markdown links are [label](url); a source marker is a [label] that
    # is NOT followed by "(", so those are excluded rather than guessed at.
    hits <- gregexpr("[[][^]]{2,60}[]](?![(])", x, perl = TRUE)[[1]]
    if (hits[1] == -1) return(NA_character_)
    last <- length(hits)
    marker <- substr(x, hits[last], hits[last] + attr(hits, "match.length")[last] - 1L)
    gsub("^[[]|[]]$", "", marker)
  }, character(1), USE.NAMES = FALSE)
}

#' Render one ruling's Markdown body as sanitised HTML
#'
#' NetrunnerDB ruling text is Markdown -- blockquotes for the answer,
#' inline links to other cards. Rendered rather than shown raw, because
#' raw would put a literal `[Foo](https://...)` in front of a reader.
#'
#' SANITISING IS DONE HERE, ON THE INPUT, because commonmark 2.0.0
#' removed its `sanitize` argument and passes embedded HTML through
#' untouched -- `markdown_html("<script>x</script>")` returns that script
#' tag intact. This is third-party text arriving over the network into a
#' page, so HTML tags are stripped from the source before rendering.
#'
#' Stripping `<...>` from the SOURCE rather than escaping it: escaping
#' would turn `&gt;` into a literal and break the blockquotes
#' NetrunnerDB writes its answers in, which is most of the formatting
#' worth keeping. The cost is that Markdown autolinks (`<https://...>`)
#' are dropped too; NetrunnerDB writes links as `[label](url)`, so
#' nothing in the mirrored corpus relies on them today.
#'
#' @param text Character. One ruling body.
#' @return A shiny tag, or NULL for empty text.
#' @keywords internal
ruling_body_html <- function(text) {
  if (is.na(text) || !nzchar(text)) return(NULL)
  without_tags <- gsub("<[^>]*>", "", text)
  html <- commonmark::markdown_html(without_tags, extensions = FALSE)
  shiny::div(class = "nr-ruling-body", shiny::HTML(html))
}

#' The rulings panel for one card
#'
#' Renders nothing at all when the card has none: an empty panel headed
#' "Rulings" reads as "there are no rulings", which is a claim this cannot
#' make -- the mirror carries what NetrunnerDB had at sync time, not a
#' guarantee of completeness.
#'
#' @param rulings The active nrdb `ruling` table, or NULL.
#' @param title Character. The card's title.
#' @return A shiny tag, or NULL.
#' @keywords internal
card_rulings_ui <- function(rulings, title) {
  # The gate is checked BEFORE the guard is called. A closed gate is not
  # an error -- it is the feature not being shipped yet -- so it must not
  # abort the card-detail modal around it.
  if (!isTRUE(NRDB_ATTRIBUTION_CONFIRMED)) return(NULL)

  hits <- rulings_for_card(rulings, title)
  if (nrow(hits) == 0) return(NULL)

  require_nrdb_attribution(NRDB_ATTRIBUTION_CONFIRMED)

  labels <- ruling_source_label(hits$ruling)
  verified <- !is.na(hits$nsg_rules_team_verified) & hits$nsg_rules_team_verified == 1L

  shiny::tagList(
    shiny::tags$h5(class = "nr-rulings-heading",
                   sprintf("Rulings (%d)", nrow(hits))),
    shiny::div(class = "nr-rulings", lapply(seq_len(nrow(hits)), function(i) {
      shiny::div(
        class = "nr-ruling",
        shiny::div(
          class = "nr-ruling-meta",
          if (!is.na(labels[i])) {
            shiny::tags$span(class = "nr-ruling-source", labels[i])
          },
          # The rules-team verification flag is NetrunnerDB's own, and it
          # is shown only when set: absence means "not marked verified",
          # not "rejected", so it gets no badge of its own.
          if (verified[i]) {
            shiny::tags$span(class = "nr-ruling-verified", "RULES TEAM VERIFIED")
          },
          if (!is.na(hits$date_update[i])) {
            shiny::tags$span(class = "nr-ruling-date", hits$date_update[i])
          }
        ),
        ruling_body_html(hits$ruling[i])
      )
    })),
    nrdb_disclaimer_ui()
  )
}

#' The empty rulings frame, so a missing release has the same shape as a
#' card nobody has ruled on
#' @keywords internal
empty_rulings <- function() {
  data.frame(
    title = character(0), ruling = character(0),
    date_update = character(0), nsg_rules_team_verified = integer(0),
    stringsAsFactors = FALSE
  )
}
