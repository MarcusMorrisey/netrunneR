#' Does a card carry a given cardpool subtype?
#'
#' Matches whole tokens, not substrings: `keywords` is a single
#' `" - "`-delimited string, so grepl("Icebreaker") would also match a
#' hypothetical "Icebreaker Support" subtype. NA keywords are simply no
#' subtypes, not an error.
#' @param keywords Character vector of `card.keywords` values.
#' @param subtype Character scalar to look for.
#' @return Logical vector.
#' @keywords internal
has_card_subtype <- function(keywords, subtype) {
  # Indexing rather than ifelse(): ifelse() collapses a zero-length input
  # to logical(0), which strsplit() then rejects as a non-character
  # argument. An empty card set is a legitimate input, not an error.
  keywords <- as.character(keywords)
  keywords[is.na(keywords)] <- ""
  tokens <- strsplit(keywords, " - ", fixed = TRUE)
  vapply(tokens, function(x) subtype %in% trimws(x), logical(1))
}

#' The card pool this app is about
#'
#' ICE, plus the programs that break it. For the initial release "breaks
#' it" means the `Icebreaker` subtype specifically.
#'
#' That is deliberately narrower than the eventual intent, which is every
#' card that breaks, bypasses or otherwise interacts with ICE -- Boomerang
#' (Hardware), Inside Job, Ghost Runner and so on. Those are excluded for
#' now rather than half-supported: they are not comparable on the
#' cost-to-break axis this app's matchup table is built around, and
#' several would need their own treatment in the UI. Widening the pool
#' means deciding what a "matchup" means for a card that bypasses rather
#' than breaks, which is a design question, not a filter change.
#'
#' @param cards A data frame with `type_code` and `keywords` columns.
#' @return `cards`, filtered.
#' @export
ice_breaker_pool <- function(cards) {
  keep <- cards$type_code == "ice" |
    (cards$type_code == "program" & has_card_subtype(cards$keywords, "Icebreaker"))
  cards[keep, , drop = FALSE]
}

#' Compute ice/breaker matchup pairs
#'
#' Joins ice_breaker_traits to cardpool with an inner_join, expands the
#' pairwise matrix with dplyr::cross_join() over the ice/breaker DATA
#' FRAMES (not their individual columns as separate vectors -- an earlier
#' version passed `ice_code = ice$code, ice_subtypes = ice$subtypes, ...`
#' straight to tidyr::expand_grid(), which treats each argument as an
#' independent axis of the cross product rather than columns that must
#' travel together from the same source row; that silently produced
#' every (ice_subtypes, breaker_subtypes) combination for every
#' (ice_code, breaker_code) pair, not just each code's own real subtype,
#' and duplicated any pair where more than one such combination happened
#' to pass the subtype filter -- caught by this function's first real
#' test coverage, not by inspection), filters to subtype-compatible
#' pairs and orders output by ice_code then breaker_code. Merges in
#' `matchup_overrides` (an override for a given pair always replaces the
#' formula-derived row for that pair) and stamps a `source` column
#' (`"formula"` / `"override"` / `"not_computable"`) plus a
#' `credit_differential` column (`ice.rez_cost - cost_to_break`; `NA` iff
#' `source == "not_computable"`) on every row. Emits a sidecar manifest of
#' the input release-id map, the analysis_revision from a clean
#' gert::git_status(), the renv.lock hash, the spec id, the parameters and
#' the output hashes, with built_at excluded from cache identity so a
#' re-run of the same inputs at a different wall-clock time still resolves
#' to the same cache identity.
#'
#' SCOPE. `cost_to_break` is the standard encounter: raise the breaker to
#' the ice's strength, then break every subroutine. It is not the
#' cheapest line of play -- it does not model paying a subroutine's cost
#' instead of breaking it, letting a subroutine fire, or any
#' card-specific trick. It is the number that makes two breakers
#' comparable against the same ice.
#'
#' Any pair whose inputs are not all known stays `source ==
#' "not_computable"`: an ice whose subroutine count is variable by design
#' (Ashigaru counts cards in HQ), a breaker whose break cost is a virus
#' or power counter rather than credits, or a breaker with no
#' strength-pump facing ice above its strength -- which it cannot break
#' at all, an answer that is NA rather than a large number.
#'
#' @param ice_breaker_traits A tibble from the active implementation release.
#' @param cardpool A tibble of cards from the active cardpool release.
#' @param matchup_overrides A tibble of manually-curated corrections, one
#'   row per (ice_code, breaker_code) pair, with at least `ice_code`,
#'   `breaker_code`, `cost_to_break` columns -- see
#'   `inst/extdata/matchup_overrides.csv`. `cost_to_break` must be an
#'   INTEGER column: it is folded into this function's own integer cost
#'   column, and a character one aborts inside dplyr::if_else() with a
#'   message that names neither this argument nor the file it came from.
#'   Read the packaged file with read_matchup_overrides()
#'   (`R/operations.R`), which declares the types rather than letting
#'   readr guess them from however many rows happen to be present.
#'   REQUIRED, not optional: an
#'   earlier draft of this function's documentation described merging
#'   overrides without the signature ever accepting them as an input --
#'   fixed here so the declared contract matches the documented behavior.
#' @param cardpool_release_id,implementation_release_id Character. The
#'   exact release IDs the two inputs above were read from, recorded in
#'   the manifest for provenance/cache-identity purposes only.
#'
#' @return A list with `matchups` (a tibble) and `manifest` (a sidecar list).
#' @export
compute_ice_breaker_matchups <- function(ice_breaker_traits, cardpool, matchup_overrides,
                                          cardpool_release_id = NA_character_,
                                          implementation_release_id = NA_character_) {
  traits <- dplyr::inner_join(ice_breaker_traits, cardpool, by = "code")

  ice <- dplyr::filter(traits, .data$type_code == "ice")
  # Icebreakers only, not every program (see ice_breaker_pool()). This
  # previously took ALL programs, so the cross-join paired every piece of
  # ICE with Datasucker, Self-modifying Code and every other non-breaker
  # in the pool.
  breakers <- dplyr::filter(
    traits,
    .data$type_code == "program" & has_card_subtype(.data$keywords, "Icebreaker")
  )

  pairs <- dplyr::cross_join(
    dplyr::select(ice, ice_code = "code", ice_keywords = "keywords",
                  ice_rez_cost = "cost", ice_strength = "strength",
                  ice_subroutines = "subroutine_count"),
    dplyr::select(breakers, breaker_code = "code", breaker_strength = "strength",
                  "break_cost", "break_qty", "break_subtype",
                  "pump_cost", "pump_amount")
  )

  # Filtered on the breaker's OWN declared break subtype, not on a shared
  # word between the two cards' printed keywords -- see
  # breaker_matches_ice().
  pairs <- dplyr::filter(pairs, breaker_matches_ice(.data$ice_keywords, .data$break_subtype))

  pairs$cost_to_break <- compute_cost_to_break_formula(
    pairs$ice_strength, pairs$ice_subroutines, pairs$breaker_strength,
    pairs$break_cost, pairs$break_qty, pairs$pump_cost, pairs$pump_amount
  )
  pairs$source <- dplyr::if_else(is.na(pairs$cost_to_break), "not_computable", "formula")

  override_lookup <- dplyr::transmute(
    matchup_overrides,
    ice_code = .data$ice_code, breaker_code = .data$breaker_code,
    override_cost = .data$cost_to_break
  )
  matchups <- dplyr::left_join(pairs, override_lookup, by = c("ice_code", "breaker_code"))
  matchups <- dplyr::mutate(
    matchups,
    source = dplyr::if_else(!is.na(.data$override_cost), "override", .data$source),
    cost_to_break = dplyr::if_else(!is.na(.data$override_cost), .data$override_cost, .data$cost_to_break),
    credit_differential = dplyr::if_else(
      .data$source == "not_computable", NA_integer_, .data$ice_rez_cost - .data$cost_to_break
    )
  )

  matchups <- dplyr::arrange(
    dplyr::select(matchups, "ice_code", "breaker_code", "cost_to_break", "credit_differential", "source"),
    .data$ice_code, .data$breaker_code
  )

  overrides_content_hash <- digest::digest(matchup_overrides, algo = "sha256")

  manifest <- build_view_manifest(
    spec_id = "ice-breaker-matchups-v2",
    output = matchups,
    parameters = list(
      cardpool_release_id = cardpool_release_id,
      implementation_release_id = implementation_release_id,
      overrides_content_hash = overrides_content_hash
    )
  )

  list(matchups = matchups, manifest = manifest)
}

#' The real cost-to-break formula
#'
#' What a runner pays to get through one piece of ice with one breaker,
#' once: raise the breaker to the ice's strength, then break every
#' subroutine.
#'
#'     pumps  = ceiling(max(0, ice_strength - breaker_strength) / pump_amount)
#'     breaks = ceiling(subroutine_count / break_qty)
#'     cost   = pumps * pump_cost + breaks * break_cost
#'
#' `break_qty == 0` means "break all subroutines" (how this codebase
#' writes cards like Begemot), so it costs one application whatever the
#' subroutine count.
#'
#' This is the standard encounter, not the cheapest line of play. It does
#' not model paying a subroutine's own cost instead of breaking it,
#' letting a subroutine fire, derezzing, or any card-specific trick --
#' those are decisions, not arithmetic. It is the number that makes two
#' breakers comparable against the same ice, which is what the board asks
#' for.
#'
#' EVERY UNKNOWN PROPAGATES TO NA, and NA becomes `not_computable`
#' upstream. A breaker with no `strength-pump` cannot reach ice above its
#' own strength at all, and a strength it cannot reach is `NA` rather
#' than a large number -- "you cannot do this" and "this is expensive"
#' are different answers.
#'
#' @param ice_strength,ice_subroutines Integer vectors, from cardpool and
#'   the implementation lineage respectively.
#' @param breaker_strength,break_cost,break_qty,pump_cost,pump_amount
#'   Integer vectors for the breaker side.
#' @return An integer vector of credit costs, `NA_integer_` where any
#'   input needed for that pair is unknown.
compute_cost_to_break_formula <- function(ice_strength, ice_subroutines,
                                          breaker_strength, break_cost,
                                          break_qty, pump_cost, pump_amount) {
  gap <- pmax(0L, ice_strength - breaker_strength)

  # A breaker already at or above the ice's strength pays nothing to get
  # there, even if it has no pump at all -- so the pump columns are only
  # required where the gap is positive.
  # The divisors are made NA BEFORE dividing, not tested inside ifelse():
  # ifelse() evaluates both branches over the whole vector, so a guard in
  # the condition does not stop the division happening, and x/0 is Inf,
  # which as.integer() then warns about while quietly producing NA.
  safe_pump_amount <- ifelse(is.na(pump_amount) | pump_amount <= 0L, NA_integer_, pump_amount)
  pumps <- ifelse(gap == 0L, 0L, as.integer(ceiling(gap / safe_pump_amount)))
  pump_total <- ifelse(pumps == 0L, 0L, pumps * pump_cost)

  # break_qty 0 is "all subroutines at once", so one application. A
  # subroutine count of 0 costs nothing to break, which is right: there
  # is nothing there.
  safe_break_qty <- ifelse(is.na(break_qty) | break_qty <= 0L, NA_integer_, break_qty)
  applications <- ifelse(
    is.na(break_qty) | is.na(ice_subroutines), NA_integer_,
    ifelse(break_qty == 0L, 1L, as.integer(ceiling(ice_subroutines / safe_break_qty)))
  )
  applications <- ifelse(!is.na(ice_subroutines) & ice_subroutines == 0L, 0L, applications)
  break_total <- applications * break_cost

  as.integer(pump_total + break_total)
}

#' Can this breaker break this ice at all?
#'
#' Uses the breaker's OWN declared break subtype, read from the
#' implementation source, rather than intersecting the two cards'
#' printed keyword strings. The old check looked for any shared word
#' between an ice's keywords and a breaker's, which is not what breaking
#' means: it counted "AP" or "Bioroid" as a match, and it had no way to
#' express an AI breaker that breaks anything.
#'
#' `"All"` is how the source writes an AI breaker (Atman, Aumakua), and
#' it matches every ice.
#'
#' @param ice_keywords Character vector of the ice's `" - "`-delimited
#'   keyword string, from cardpool.
#' @param break_subtype Character vector of the breaker's declared break
#'   subtype, from the implementation lineage.
#' @return A logical vector. `NA` break_subtype is FALSE: a breaker whose
#'   break clause could not be read is not assumed to break anything.
#' @keywords internal
breaker_matches_ice <- function(ice_keywords, break_subtype) {
  vapply(seq_along(break_subtype), function(i) {
    st <- break_subtype[[i]]
    if (is.na(st)) return(FALSE)
    if (identical(st, "All")) return(TRUE)
    has_card_subtype(ice_keywords[[i]], st)
  }, logical(1))
}

#' Build a derived-view sidecar manifest
#'
#' Records the analysis_revision from a clean gert::git_status(), the
#' renv.lock hash, the spec id, the parameters and the output hash.
#' built_at is recorded for humans but excluded from the cache identity
#' digest, so re-running the same inputs at a different time still
#' resolves to the same cache identity.
#'
#' Resolves pkg-src the same way build_revision() (R/build-revision.R)
#' does: pkg_root/pkg-src if it exists (an installed package, where R CMD
#' INSTALL has stripped R/*.R and renv.lock from pkg_root itself), else
#' pkg_root directly (devtools::test(), R CMD check on the source tree,
#' where renv.lock is already sitting at pkg_root). An earlier version of
#' this function hardcoded file.path(pkg_root, "pkg-src") unconditionally
#' -- unlike build_revision()'s own fallback -- so it aborted under
#' devtools::test() with no pkg-src staged, which was never a real
#' requirement in that context; nothing had called
#' compute_ice_breaker_matchups() (the only caller of this function) in a
#' test before this branch surfaced it.
#' @keywords internal
build_view_manifest <- function(spec_id, output, parameters) {
  pkg_root <- find_package_root()
  pkg_src_dir <- file.path(pkg_root, "pkg-src")
  src_root <- if (fs::dir_exists(pkg_src_dir)) pkg_src_dir else pkg_root

  status <- tryCatch(gert::git_status(repo = pkg_root), error = function(e) NULL)
  analysis_revision <- if (!is.null(status) && nrow(status) == 0) {
    gert::git_commit_info(repo = pkg_root)$id
  } else {
    NA_character_
  }

  output_hash <- digest::digest(output, algo = "sha256")
  lock_hash <- renv_lock_hash(src_root)

  cache_identity <- digest::digest(
    list(spec_id = spec_id, parameters = parameters, analysis_revision = analysis_revision,
         renv_lock_hash = lock_hash, output_hash = output_hash),
    algo = "sha256"
  )

  list(
    spec_id = spec_id,
    parameters = parameters,
    analysis_revision = analysis_revision,
    renv_lock_hash = lock_hash,
    output_hash = output_hash,
    cache_identity = cache_identity,
    built_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}
