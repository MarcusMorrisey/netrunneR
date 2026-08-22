#' Fetch and check out a git-mirror lineage's configured ref
#'
#' Fetches and checks out the configured ref with gert into the attempt
#' directory, treating the checkout itself as raw. Reads source_revision
#' from gert::git_commit_info() and forms the composite release_id of
#' source_revision and build_revision. Sends no throttle pacing, since the
#' upstream for cardpool and implementation is GitHub rather than a
#' volunteer-run server.
#'
#' @param lineage A lineage object of class netrunneR_git_mirror.
#' @param attempt_dir Character. Staging directory for this sync attempt.
#' @param ... Ignored.
#'
#' @return A list with `raw_dir`, `source_revision` and `content_identity`.
#' @export
fetch_lineage.netrunneR_git_mirror <- function(lineage, attempt_dir, ...) {
  raw_dir <- file.path(attempt_dir, "raw")
  fs::dir_create(raw_dir)

  repo_url <- lineage$repo_url
  ref <- if (is.null(lineage$ref)) "main" else lineage$ref

  # Git-mirror fetch needs no network throttling or personal-data
  # handling, unlike the abr/nrdb api_poll lineages' request pacing and
  # allowlist/deny-pattern checks.
  gert::git_clone(repo_url, path = raw_dir)
  gert::git_branch_checkout(ref, repo = raw_dir)

  commit_info <- gert::git_commit_info(repo = raw_dir)
  source_revision <- commit_info$id

  list(
    raw_dir = raw_dir,
    source_revision = source_revision,
    content_identity = source_revision
  )
}
