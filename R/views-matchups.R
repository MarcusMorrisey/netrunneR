#' Compute ice/breaker matchup pairs
#'
#' Joins ice_breaker_traits to cardpool with an inner_join, expands the
#' pairwise matrix with tidyr::expand_grid(), filters to subtype-compatible
#' pairs and orders output by ice_code then breaker_code. Emits a sidecar
#' manifest of the input release-id map, the analysis_revision from a
#' clean gert::git_status(), the renv.lock hash, the spec id, the
#' parameters and the output hashes, with built_at excluded from cache
#' identity so a re-run of the same inputs at a different wall-clock time
#' still resolves to the same cache identity.
#'
#' @param ice_breaker_traits A tibble from the implementation release.
#' @param cardpool A tibble of cards from the cardpool release.
#'
#'
#' @return A list with `matchups` (a tibble) and `manifest` (a sidecar list).
#' @export
compute_ice_breaker_matchups <- function(ice_breaker_traits, cardpool) {
  traits <- dplyr::inner_join(ice_breaker_traits, cardpool, by = "code")

  ice <- dplyr::filter(traits, .data$type_code == "ice")
  breakers <- dplyr::filter(traits, .data$type_code == "program")

  pairs <- tidyr::expand_grid(
    ice_code = ice$code, ice_subtypes = ice$subtypes,
    breaker_code = breakers$code, breaker_subtypes = breakers$subtypes
  )

  pairs <- dplyr::filter(pairs, purrr::map2_lgl(.data$ice_subtypes, .data$breaker_subtypes, subtype_compatible))
  pairs <- dplyr::arrange(pairs, .data$ice_code, .data$breaker_code)
  matchups <- dplyr::select(pairs, "ice_code", "breaker_code")

  manifest <- build_view_manifest(
    spec_id = "ice-breaker-matchups-v1",
    output = matchups,
    parameters = list()
  )

  list(matchups = matchups, manifest = manifest)
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
