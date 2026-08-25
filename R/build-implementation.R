#' Extract normalized ice/breaker trait rows from the implementation tree
#'
#' Extracts a normalized ice_breaker_traits tibble from the Jinteki
#' implementation tree, one row per card-definition file, carrying
#' subtypes, base strength and per-subtype break cost. Cross-checks codes
#' against cardpool in both directions as a counted warning that never
#' blocks promotion, since implementation and cardpool are fetched
#' independently and are expected to drift briefly around a new card's
#' release.
#'
#' The real mtgred/netrunner tree (confirmed live this session) is a
#' Clojure project with card definitions grouped one file per card
#' *category* under src/clj/game/cards/ (agendas.clj, ice.clj,
#' programs.clj, ...) -- there is no server/cards/*.js per-card layout.
#' extract_ice_breaker_traits() below is still the pre-existing stub (it
#' has never parsed real trait data out of a definition file's contents,
#' only derived a placeholder `code` from the filename), so pointed at
#' the real path it now produces one placeholder row per category file
#' rather than per card. Real per-card trait extraction would mean
#' parsing individual defcard forms out of Clojure source and is future
#' work, not something this fix invents.
#'
#' @param lineage A lineage object of class netrunneR_git_mirror named "implementation".
#' @param staged_raw The value returned by fetch_lineage.netrunneR_git_mirror().
#'
#' @keywords internal
build_implementation <- function(lineage, staged_raw) {
  raw_dir <- staged_raw$raw_dir
  card_def_files <- fs::dir_ls(file.path(raw_dir, "src", "clj", "game", "cards"), recurse = TRUE, glob = "*.clj")

  traits <- purrr::map_dfr(card_def_files, extract_ice_breaker_traits)
  traits <- dplyr::arrange(traits, .data$code)

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

#' Stub extraction of ice/breaker traits for one card-definition file
#' @param path Character. Path to a card definition JS file.
#' @return A one-row tibble of ice/breaker trait columns for the card.
#' @keywords internal
extract_ice_breaker_traits <- function(path) {
  tibble::tibble(
    code = tools::file_path_sans_ext(basename(path)),
    subtypes = NA_character_,
    base_strength = NA_integer_,
    break_cost = NA_integer_
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
