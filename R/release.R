# Release resolution and release_id entropy helpers shared by run_sync(),
# promote() and rollback().
#' Resolve the active release exactly once
#'
#' Resolves active with a single fs::path_real() call and derives both the
#' raw and processed paths from that one capture, so no consumer can
#' resolve latest and processed/current independently and observe two
#' different releases if a promote() lands between two separate
#' resolutions.
#'
#' @param lineage A lineage object.
#'
#' @return A list with `release_dir`, `raw_dir` and `processed_dir`.
#' @export
resolve_release <- function(lineage) {
  active_link <- file.path(lineage$store_root, "active")

  if (!fs::file_exists(active_link)) {
    rlang::abort(
      sprintf("No active release for lineage '%s'", lineage$name),
      class = "netrunneR_no_active_release"
    )
  }

  release_dir <- fs::path_real(active_link)

  list(
    release_dir = release_dir,
    raw_dir = file.path(release_dir, "raw"),
    processed_dir = file.path(release_dir, "processed")
  )
}

#' An 8-hex entropy suffix for a plain-timestamp release_id
#'
#' Used by the three lineages whose release_id is a bare
#' <UTC timestamp>-<8 hex> (nrdb, abr, rules) rather than the composite
#' <source_revision>-b<build_revision> format the two git-mirror lineages
#' use: a bare timestamp alone risks same-second release_id collisions.
#' @keywords internal
release_entropy_suffix <- function() {
  substr(digest::digest(list(Sys.time(), stats::runif(1)), algo = "sha1"), 1, 8)
}
