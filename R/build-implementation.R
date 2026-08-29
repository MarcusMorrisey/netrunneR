#' Extract ice/breaker economics from the implementation tree
#'
#' The mtgred/netrunner tree is a Clojure project with card definitions
#' grouped one file per card CATEGORY under src/clj/game/cards/. Only
#' ice.clj and programs.clj matter here; the other categories define
#' cards that never take part in an ice/breaker encounter.
#'
#' See extract_traits_from_file() below for what is read and what is
#' deliberately left NA.
#'
#' @param lineage A lineage object of class netrunneR_git_mirror named "implementation".
#' @param staged_raw The value returned by fetch_lineage.netrunneR_git_mirror().
#'
#' @keywords internal
build_implementation <- function(lineage, staged_raw) {
  raw_dir <- staged_raw$raw_dir
  cards_dir <- file.path(raw_dir, "src", "clj", "game", "cards")

  # Only these two categories. Reading agendas.clj or assets.clj would
  # produce rows for cards that cannot appear in an ice/breaker encounter
  # -- which is exactly what the previous stub did, one placeholder row
  # per category file.
  traits <- dplyr::bind_rows(
    extract_traits_from_file(file.path(cards_dir, "ice.clj"), "ice"),
    extract_traits_from_file(file.path(cards_dir, "programs.clj"), "program")
  )

  # Codes come from cardpool, because a defcard is keyed by title. This
  # is the one place implementation reads cardpool, and it degrades to an
  # empty table rather than failing if no cardpool release is active --
  # the cross-check below then reports it.
  cardpool <- cardpool_card_titles()
  traits <- attach_cardpool_codes(traits, cardpool)
  traits <- dplyr::select(
    traits, "code", "title", "kind", "subroutine_count",
    "break_cost", "break_qty", "break_subtype", "pump_cost", "pump_amount",
    "pump_stealth", "pump_resource_type", "pump_resource_qty",
    "parse_status"
  )

  db_path <- file.path(dirname(raw_dir), "processed", "implementation.sqlite")
  fs::dir_create(dirname(db_path))
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  apply_schema(con, "implementation")

  DBI::dbWithTransaction(con, {
    DBI::dbWriteTable(con, "ice_breaker_traits", traits, append = TRUE)
  })

  cross_check <- cross_check_cardpool_codes(traits$code, cardpool_codes())

  br <- build_revision(lineage, build_module_path = "R/build-implementation.R")

  list(
    db_path = db_path,
    build_revision = br,
    # Composite <source_revision>-b<build_revision prefix>, matching
    # build-cardpool.R's release_id shape: both git-mirror lineages key
    # release identity to the exact commit fetched.
    release_id = sprintf("%s-b%s", staged_raw$source_revision, substr(br, 1, 12)),
    checks = list(cross_check)
  )
}

#' Cross-check ice/breaker codes against the cardpool release, both directions
#' @param implementation_codes Character vector of codes found in the
#'   implementation tree.
#' @param cardpool_codes Character vector of codes in the active cardpool
#'   release.
#' @return A check-result list with `check`, `status`, and `message`.
#' @keywords internal
cross_check_cardpool_codes <- function(implementation_codes, cardpool_codes) {
  missing_in_implementation <- setdiff(cardpool_codes, implementation_codes)
  missing_in_cardpool <- setdiff(implementation_codes, cardpool_codes)
  n_mismatch <- length(missing_in_implementation) + length(missing_in_cardpool)

  list(
    check = "cross_lineage_code_match",
    status = if (n_mismatch > 0) "warn" else "pass",
    message = sprintf(
      "%d codes in cardpool missing from implementation; %d codes in implementation missing from cardpool",
      length(missing_in_implementation), length(missing_in_cardpool)
    )
  )
}

#' Codes present in the active cardpool release, or empty if unavailable
#' @return A character vector of card codes.
#' @keywords internal
cardpool_codes <- function() {
  result <- query_active_release("cardpool", "cardpool.sqlite", "SELECT code FROM card")
  if (is.null(result)) return(character(0))
  result$data$code
}

#' Code/title pairs from the active cardpool release, or an empty frame
#'
#' The implementation lineage needs titles, not just codes, because a
#' Clojure defcard names the card rather than the printing.
#' @return A data frame with `code` and `title`, empty if no release is
#'   active.
#' @keywords internal
cardpool_card_titles <- function() {
  result <- query_active_release("cardpool", "cardpool.sqlite", "SELECT code, title FROM card")
  if (is.null(result)) return(data.frame(code = character(0), title = character(0)))
  result$data
}

#' Helper macros whose subroutine count is known without reading them
#'
#' These wrap a subroutine list rather than declaring `:subroutines`
#' directly, so `count_vector_after()` finds nothing. Each entry was read
#' out of the helper's own definition in ice.clj:
#'
#' * `wall-ice [subroutines]` sets `:subroutines subroutines` -- count the
#'   vector it is handed.
#' * `grail-ice [ability]` sets `:subroutines [ability]` -- always one.
#' * `morph-ice [base other ability]` likewise -- always one.
#'
#' `space-ice` is varargs (`(vec abilities)`) rather than a vector, and
#' `constellation-ice` wraps a further helper, so neither is listed:
#' getting them right means reading two more macros, and a wrong
#' subroutine count is worse than a missing one.
#'
#' Deliberately NOT here: `variable-subs-ice`, and any card registering
#' `:additional-subroutines`. Those have no fixed count by design (Ashigaru
#' counts cards in HQ, Komainu counts the grip), so NA is the true answer.
#' @keywords internal
ICE_SUBROUTINE_HELPERS <- list(
  `wall-ice` = NA_integer_,  # count the vector argument
  `grail-ice` = 1L,
  `morph-ice` = 1L
)

#' Subroutine count for one ice defcard, or NA
#' @keywords internal
ice_subroutine_count <- function(body) {
  n <- count_vector_after(body, ":subroutines")
  if (!is.na(n)) return(n)

  for (helper in names(ICE_SUBROUTINE_HELPERS)) {
    pattern <- paste0("\\(", helper, "[ \n]")
    if (!grepl(pattern, body)) next
    fixed_count <- ICE_SUBROUTINE_HELPERS[[helper]]
    if (!is.na(fixed_count)) return(fixed_count)
    # wall-ice: count the vector it is passed.
    ch <- strsplit(body, "", fixed = TRUE)[[1]]
    at <- regexpr(pattern, body)
    for (i in seq(at, length(ch))) {
      if (ch[i] == "[") return(count_form_elements(ch, i))
    }
  }
  NA_integer_
}

#' Why an ice's subroutine count could not be read
#' @keywords internal
ice_parse_status <- function(body, count) {
  if (!is.na(count)) return("parsed")
  if (grepl("variable-subs-ice", body, fixed = TRUE)) return("variable_subroutines")
  if (grepl(":additional-subroutines", body, fixed = TRUE)) return("variable_subroutines")
  "unreadable_form"
}

#' Break and pump economics for one program defcard, or NAs
#'
#' Only the plain-credit forms are read. `(break-sub 1 1 "Barrier")` is
#' "1 credit breaks 1 Barrier subroutine"; `(strength-pump 1 1)` is "1
#' credit for +1 strength". A cost expressed as a power counter, virus
#' counter, trash, stealth credit or X is NOT a plain credit cost, so it
#' is left NA -- charging the runner credits for a card that spends virus
#' counters would be a wrong number, which is worse than no number.
#'
#' A `break_qty` of 0 means "break all subroutines", which is how this
#' codebase writes cards like Begemot.
#' @keywords internal
breaker_economics <- function(body) {
  none <- list(break_cost = NA_integer_, break_qty = NA_integer_,
               break_subtype = NA_character_, pump_cost = NA_integer_,
               pump_amount = NA_integer_, pump_stealth = NA_integer_,
               pump_resource_type = NA_character_, pump_resource_qty = NA_integer_,
               parse_status = "not_a_breaker")

  if (!grepl("(break-sub ", body, fixed = TRUE)) return(none)

  # The subtype is read even when the COST is not. A breaker whose cost is
  # a virus counter still declares what it breaks, and keeping that means
  # the pair survives into the matchup table as an explicit
  # "not_computable" instead of vanishing from it -- a pair that is absent
  # and a pair that is unknown look identical to a reader, and only one of
  # them is true here.
  # Read the whole (break-sub ...) form and take its last string literal,
  # rather than matching up to the first quote: a cost argument like
  # [(->c :virus 1)] contains its own parentheses, so any regex bounded by
  # ")" stops inside the cost and never reaches the subtype.
  declared_subtype <- break_sub_subtype(body)

  bs <- regmatches(body, regexpr('\\(break-sub +[0-9]+ +[0-9]+ +"[^"]*"', body))
  if (length(bs) == 0) {
    none$break_subtype <- declared_subtype
    none$parse_status <- "non_credit_break_cost"
    # The PUMP is still read even though the BREAK cost was not. The two
    # are independent clauses, and a card whose break cost is a virus
    # counter can still have a perfectly ordinary credit pump -- dropping
    # it here would lose data for no reason.
    p <- pump_economics(body)
    none$pump_cost <- p$pump_cost
    none$pump_amount <- p$pump_amount
    none$pump_stealth <- p$pump_stealth
    none$pump_resource_type <- p$pump_resource_type
    none$pump_resource_qty <- p$pump_resource_qty
    return(none)
  }
  bsn <- as.integer(regmatches(bs, gregexpr("[0-9]+", bs))[[1]])
  subtype <- sub('.*"([^"]*)"$', "\\1", bs)

  p <- pump_economics(body)

  list(
    break_cost = bsn[1], break_qty = bsn[2], break_subtype = subtype,
    pump_cost = p$pump_cost, pump_amount = p$pump_amount,
    pump_stealth = p$pump_stealth,
    pump_resource_type = p$pump_resource_type,
    pump_resource_qty = p$pump_resource_qty,
    # A breaker with no strength-pump has a fixed strength, which is a
    # real card design (Atman, Adept), not a parse failure. It only
    # limits which ice it can reach, which the formula handles.
    #
    # Afterimage used to be named here as an example of that and was the
    # wrong example: it has a pump, costing a stealth credit, which the
    # old two-integer regex could not see. See pump_economics().
    parse_status = if (p$has_pump) "parsed" else "parsed_no_pump"
  )
}

#' Read a `(strength-pump ...)` cost, whatever form it takes
#'
#' The pump clause has four shapes in the source, and only the first was
#' ever read:
#'
#'     (strength-pump 2 2)                             plain credits
#'     (strength-pump (->c :credit 1 {:stealth 1}) 3)  credits, stealth-sourced
#'     (strength-pump [(->c :power 1)] 2)              a non-credit resource
#'     (strength-pump [(->c :virus 1) 3] 2)            both at once (Hantu)
#'
#' A regex bounded to two integers matches only the first, so the other
#' three produced no pump at all -- and compute_cost_to_break_formula()
#' reads "no pump" as "this breaker cannot reach ice above its own
#' strength", which is a DEFINITE WRONG ANSWER rather than a missing one.
#' Eleven breakers were affected, every stealth breaker among them
#' (Refractor, Switchblade, Dagger, Blackstone, Houdini, Penrose,
#' Afterimage) plus Audrey v2, Faust, Hantu and Propeller. That is the
#' failure this whole table exists to avoid: an honest NA says "we do not
#' know", and these were saying "it cannot be done".
#'
#' STEALTH CREDITS ARE STILL CREDITS. `{:stealth 1}` means one of the
#' credits must come from a stealth card, and `:all-stealth` means all of
#' them must; neither changes how many credits are paid. So the count
#' goes in `pump_cost` as normal and the sourcing constraint is recorded
#' separately in `pump_stealth`, rather than inventing a second currency.
#' Power and virus counters and trashing a card are NOT credits, and are
#' kept out of `pump_cost` entirely for the same reason -- adding them in
#' would be a conversion rate nobody has defined.
#'
#' @param body Character. One defcard body.
#' @return A list with `pump_cost` (credits), `pump_amount` (strength
#'   gained), `pump_stealth` (how many of those credits must be stealth,
#'   NA if none need be), `pump_resource_type`/`pump_resource_qty` (a
#'   non-credit cost, NA if there is none) and `has_pump`.
#' @keywords internal
pump_economics <- function(body) {
  none <- list(pump_cost = NA_integer_, pump_amount = NA_integer_,
               pump_stealth = NA_integer_, pump_resource_type = NA_character_,
               pump_resource_qty = NA_integer_, has_pump = FALSE)

  at <- regexpr("(strength-pump ", body, fixed = TRUE)
  if (at == -1) return(none)
  ch <- strsplit(body, "", fixed = TRUE)[[1]]
  close <- read_form(ch, at)
  if (is.na(close)) return(none)
  form <- paste(ch[at:close], collapse = "")

  # The plain form, which is 82 of the 93 breakers that have a pump.
  plain <- regmatches(form, regexpr("^\\(strength-pump +[0-9]+ +[0-9]+", form))
  if (length(plain)) {
    n <- as.integer(regmatches(plain, gregexpr("[0-9]+", plain))[[1]])
    return(list(pump_cost = n[[1]], pump_amount = n[[2]],
                pump_stealth = NA_integer_, pump_resource_type = NA_character_,
                pump_resource_qty = NA_integer_, has_pump = TRUE))
  }

  # Otherwise the first argument is a cost form -- either one (->c ...)
  # or a vector of them -- and it is read with read_form() rather than a
  # regex because it contains its own parentheses and braces.
  inner <- sub("^\\(strength-pump[[:space:]]+", "", form)
  ich <- strsplit(inner, "", fixed = TRUE)[[1]]
  if (!length(ich) || !(ich[[1]] %in% c("(", "["))) return(none)
  cost_end <- read_form(ich, 1L)
  if (is.na(cost_end) || cost_end >= length(ich)) return(none)

  cost_txt <- paste(ich[1:cost_end], collapse = "")
  rest <- paste(ich[(cost_end + 1L):length(ich)], collapse = "")

  # The strength gained is the next integer after the cost form. Taken
  # from `rest` and not from the whole form, so a number inside the cost
  # cannot be mistaken for it.
  amt <- regmatches(rest, regexpr("[0-9]+", rest))
  if (!length(amt)) return(none)

  costs <- regmatches(cost_txt, gregexpr("\\(->c +:[a-z-]+ +[0-9]+", cost_txt))[[1]]
  if (!length(costs)) return(none)
  types <- sub("\\(->c +:([a-z-]+) +[0-9]+", "\\1", costs)
  qtys <- as.integer(sub("\\(->c +:[a-z-]+ +([0-9]+)", "\\1", costs))

  # A bare integer sitting beside the ->c forms is a plain credit cost
  # (Hantu pays a virus counter AND three credits). It is read only after
  # the ->c forms are stripped, or their own amounts would be counted a
  # second time.
  bare_txt <- gsub("\\(->c[^)]*\\)", "", cost_txt)
  bare <- as.integer(regmatches(bare_txt, gregexpr("[0-9]+", bare_txt))[[1]])

  credit_i <- which(types == "credit")
  other_i <- which(types != "credit")
  credits <- sum(c(qtys[credit_i], bare))

  stealth <- NA_integer_
  st <- regmatches(cost_txt, regexpr("\\{:stealth +(:all-stealth|[0-9]+)\\}", cost_txt))
  if (length(st)) {
    stealth <- if (grepl("all-stealth", st, fixed = TRUE)) {
      as.integer(credits)
    } else {
      as.integer(regmatches(st, regexpr("[0-9]+", st)))
    }
  }

  list(
    pump_cost = as.integer(credits),
    pump_amount = as.integer(amt),
    pump_stealth = stealth,
    pump_resource_type = if (length(other_i)) types[[other_i[[1]]]] else NA_character_,
    pump_resource_qty = if (length(other_i)) qtys[[other_i[[1]]]] else NA_integer_,
    has_pump = TRUE
  )
}

#' Extract every trait row from one card-definition file
#'
#' @param path Character. Path to a `.clj` file under `src/clj/game/cards`.
#' @param kind Character. `"ice"` or `"program"`; other files yield no rows.
#' @return A tibble of trait rows keyed by card TITLE (codes are attached
#'   later, by [attach_cardpool_codes()]).
#' @keywords internal
extract_traits_from_file <- function(path, kind) {
  # A category file that is not there yields no rows rather than an
  # error. The upstream tree reorganises occasionally, and losing one
  # category should degrade the table -- which the cross-check then
  # reports -- not abort the build for every lineage behind it.
  if (!fs::file_exists(path)) return(empty_traits())
  defs <- read_defcards(path)
  if (length(defs) == 0) return(empty_traits())

  rows <- lapply(defs, function(d) {
    if (identical(kind, "ice")) {
      n <- ice_subroutine_count(d$body)
      tibble::tibble(
        title = d$title, kind = "ice",
        subroutine_count = as.integer(n),
        break_cost = NA_integer_, break_qty = NA_integer_,
        break_subtype = NA_character_,
        pump_cost = NA_integer_, pump_amount = NA_integer_,
        pump_stealth = NA_integer_, pump_resource_type = NA_character_,
        pump_resource_qty = NA_integer_,
        parse_status = ice_parse_status(d$body, n)
      )
    } else {
      e <- breaker_economics(d$body)
      tibble::tibble(
        title = d$title, kind = "program",
        subroutine_count = NA_integer_,
        break_cost = e$break_cost, break_qty = e$break_qty,
        break_subtype = e$break_subtype,
        pump_cost = e$pump_cost, pump_amount = e$pump_amount,
        pump_stealth = e$pump_stealth,
        pump_resource_type = e$pump_resource_type,
        pump_resource_qty = e$pump_resource_qty,
        parse_status = e$parse_status
      )
    }
  })
  out <- dplyr::bind_rows(rows)
  # A program with no break-sub at all is not an icebreaker; it has no
  # place in a table about ice/breaker interaction.
  if (identical(kind, "program")) {
    out <- out[out$parse_status != "not_a_breaker", , drop = FALSE]
  }
  out
}

#' The empty trait table, so every path returns the same shape
#' @keywords internal
empty_traits <- function() {
  tibble::tibble(
    title = character(0), kind = character(0),
    subroutine_count = integer(0), break_cost = integer(0),
    break_qty = integer(0), break_subtype = character(0),
    pump_cost = integer(0), pump_amount = integer(0),
    pump_stealth = integer(0), pump_resource_type = character(0),
    pump_resource_qty = integer(0),
    parse_status = character(0)
  )
}

#' Expand title-keyed trait rows to one row per cardpool code
#'
#' A defcard names a card; the cardpool keys printings. One title can
#' therefore carry several codes, and each gets the same traits -- the
#' behaviour is a property of the card, not of the printing.
#'
#' A title with no cardpool code is dropped rather than kept with a NULL
#' key: the cross-check below counts it, so it is reported rather than
#' silently lost.
#'
#' @param traits Title-keyed trait tibble.
#' @param cardpool A data frame with `code` and `title` columns.
#' @return The same rows, keyed by `code`.
#' @keywords internal
attach_cardpool_codes <- function(traits, cardpool) {
  if (nrow(traits) == 0 || nrow(cardpool) == 0) {
    return(dplyr::mutate(empty_traits(), code = character(0)))
  }
  joined <- dplyr::inner_join(
    traits, cardpool[, c("code", "title")], by = "title", relationship = "many-to-many"
  )
  dplyr::arrange(joined, .data$code)
}

#' The subtype named by a card's first `(break-sub ...)` form, or NA
#'
#' Reads the form with [read_form()] and takes its last string literal.
#' Forms that name the subtype with a symbol rather than a literal (e.g.
#' `(break-sub nil strength subtype ...)`, where it is a function
#' argument) have no literal to find, and correctly yield NA.
#' @keywords internal
break_sub_subtype <- function(body) {
  at <- regexpr("(break-sub ", body, fixed = TRUE)
  if (at == -1) return(NA_character_)
  ch <- strsplit(body, "", fixed = TRUE)[[1]]
  close <- read_form(ch, at)
  if (is.na(close)) return(NA_character_)
  form <- paste(ch[at:close], collapse = "")
  literals <- regmatches(form, gregexpr('"[^"]*"', form))[[1]]
  if (length(literals) == 0) return(NA_character_)
  gsub('"', "", literals[[length(literals)]], fixed = TRUE)
}
