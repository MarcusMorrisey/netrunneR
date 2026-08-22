# Atomic promotion: move a staged release into releases/<release_id> and
# swap the active symlink via a same-directory rename, never leaving
# active absent or pointed at a partial release.
#' Acquire the non-blocking sync lock for a lineage
#'
#' Wraps filelock::lock(timeout = 0). Contention is a successful skip, not
#' an error: the caller exits status 4 with no ledger record, never
#' treating a held lock as a sync failure.
#'
#' @param store_root Character. The lineage's store root.
#'
#' @return A filelock lock object, or NULL if the lock is held elsewhere.
#' @export
acquire_lock <- function(store_root) {
  fs::dir_create(store_root)
  lock_path <- file.path(store_root, ".sync.lock")
  filelock::lock(lock_path, timeout = 0)
}

#' Move a staged release into place and swap the active symlink
#'
#' A single fs::file_move() same-filesystem rename of the staging directory
#' to releases/<release_id>, then a symlink swap performed by creating a
#' temp symlink in the same directory as active and renaming it over
#' active. There is no window in which active is absent or points at an
#' incomplete release, because the rename(2) that repoints the symlink is
#' atomic on the same filesystem.
#' Atomicity holds only while store_root stays a same-filesystem bind mount.
#'
#' @param store_root Character. The lineage's store root.
#' @param staging_dir Character. The staged release directory.
#' @param release_id Character. The release identifier.
#'
#' @return Character. The path to the new release directory.
#' @export
promote <- function(store_root, staging_dir, release_id) {
  releases_dir <- file.path(store_root, "releases")
  fs::dir_create(releases_dir)
  release_dir <- file.path(releases_dir, release_id)

  fs::file_move(staging_dir, release_dir)
  swap_active(store_root, release_dir)

  release_dir
}

#' Re-point active at an existing release directory
#'
#' Reused by both promote() (a newly staged release) and rollback() (an
#' existing prior release): the swap itself carries no knowledge of where
#' the target release directory came from.
#'
#' @param store_root Character. The lineage's store root.
#' @param target_release_dir Character. An existing release directory.
#' @export
swap_active <- function(store_root, target_release_dir) {
  active_link <- file.path(store_root, "active")
  tmp_link <- file.path(store_root, sprintf(".active.tmp.%s", basename(tempfile())))

  fs::link_create(target_release_dir, tmp_link, symbolic = TRUE)
  fs::file_move(tmp_link, active_link)

  invisible(active_link)
}

#' Roll back active to a previously promoted release
#'
#' Re-validates that the target release exists on disk before swapping,
#' and reuses the same rename-based swap promote() uses so there is no
#' behavioral difference between promoting a new release and pointing
#' active at an older one beyond which directory already exists.
#'
#' @param lineage A lineage object.
#' @param release_id Character. An existing release_id under releases/.
#'
#' @export
rollback <- function(lineage, release_id) {
  release_dir <- file.path(lineage$store_root, "releases", release_id)
  if (!fs::dir_exists(release_dir)) {
    rlang::abort(sprintf("No such release: %s", release_id), class = "netrunneR_no_such_release")
  }
  swap_active(lineage$store_root, release_dir)
  invisible(release_dir)
}
