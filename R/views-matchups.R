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
#' NOTE ON SCOPE: `cost_to_break` cannot be computed from real per-card
#' data yet -- `extract_ice_breaker_traits()` (R/build-implementation.R)
#' is still a stub that has never parsed real subtype/strength/cost values
#' out of a mtgred/netrunner Clojure card definition; it emits one
#' placeholder row per *file* (currently one per card *category*, not per
#' card). `compute_cost_to_break_formula()` below always returns `NA`
#' until that extraction work lands, so every non-override pair correctly
#' reports `source == "not_computable"` today -- that is the honest
#' current state, not a bug this change introduces.
#'
#' There is no separate "matchup release": this function is a plain,
#' repeatedly-callable computation over already-loaded `cardpool` and
#' `implementation` release data, exactly like `compute_identity_ratings()`
#' (R/views-ratings.R) -- it is not promoted, versioned, or resolved via
#' `lineage()`/`resolve_release()`, and `matchup` is not one of the five
#' `BUILTIN_LINEAGES`. The `cardpool_release_id`/`implementation_release_id`
#' parameters below exist purely as cache/provenance metadata in the
#' sidecar manifest, not as inputs to a promote/rollback mechanism.
#'
#' @param ice_breaker_traits A tibble from the active implementation release.
#' @param cardpool A tibble of cards from the active cardpool release.
#' @param matchup_overrides A tibble of manually-curated corrections, one
#'   row per (ice_code, breaker_code) pair, with at least `ice_code`,
#'   `breaker_code`, `cost_to_break` columns -- see
#'   `inst/extdata/matchup_overrides.csv`. REQUIRED, not optional: an
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
  breakers <- dplyr::filter(traits, .data$type_code == "program")

  pairs <- dplyr::cross_join(
    dplyr::select(ice, ice_code = "code", ice_subtypes = "subtypes", ice_rez_cost = "cost"),
    dplyr::select(breakers, breaker_code = "code", breaker_subtypes = "subtypes")
  )

  pairs <- dplyr::filter(pairs, purrr::map2_lgl(.data$ice_subtypes, .data$breaker_subtypes, subtype_compatible))

  pairs$cost_to_break <- compute_cost_to_break_formula(pairs$ice_code, pairs$breaker_code, ice_breaker_traits)
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

#' Real cost-to-break formula (NOT YET IMPLEMENTED)
#'
#' Always returns NA_integer_ until extract_ice_breaker_traits()
#' (R/build-implementation.R) parses real per-card subtype/strength/cost
#' values out of the mtgred/netrunner Clojure source -- see the note on
#' compute_ice_breaker_matchups() above.
#' @return An integer vector the same length as `ice_codes`, all NA.
#' @keywords internal
compute_cost_to_break_formula <- function(ice_codes, breaker_codes, ice_breaker_traits) {
  rep(NA_integer_, length(ice_codes))
}

#' @keywords internal
subtype_compatible <- function(ice_subtypes, breaker_subtypes) {
  ice_set <- strsplit(if (is.na(ice_subtypes)) "" else ice_subtypes, " - ")[[1]]
  breaker_set <- strsplit(if (is.na(breaker_subtypes)) "" else breaker_subtypes, " - ")[[1]]
  length(intersect(ice_set, breaker_set)) > 0
}

#' Build a derived-view sidecar manifest
#'
#' Records the analysis_revision from a clean gert::git_status(), the
#' renv.lock hash, the spec id, the parameters and the output hash.
#' built_at is recorded for humans but excluded from the cache identity
#' digest, so re-running the same inputs at a different time still
#' resolves to the same cache identity.
#' @keywords internal
build_view_manifest <- function(spec_id, output, parameters) {
  pkg_root <- find_package_root()
  status <- tryCatch(gert::git_status(repo = pkg_root), error = function(e) NULL)
  analysis_revision <- if (!is.null(status) && nrow(status) == 0) {
    gert::git_commit_info(repo = pkg_root)$id
  } else {
    NA_character_
  }

  output_hash <- digest::digest(output, algo = "sha256")
  lock_hash <- renv_lock_hash(file.path(pkg_root, "pkg-src"))

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
