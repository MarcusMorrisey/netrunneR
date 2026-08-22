# Package-wide runtime policy constants and startup configuration checks.
#' Package-wide LLM use policy
#'
#' Any code path that would index nrdb-sourced free text for an LLM must
#' assert this constant with stopifnot() rather than relying on a comment,
#' so the constraint fails loudly if that code path is ever reached before
#' the policy is revisited.
#' @export
LLM_USE_POLICY <- "no_llm_indexing_of_nrdb_text"

#' Validate required runtime configuration at startup
#'
#' Rejects a missing or placeholder NRDB_CONTACT so a container never
#' fetches against NetrunnerDB with a User-Agent that does not identify a
#' real contact -- a condition upstream operators rely on to reach the
#' mirror's operator if something goes wrong.
#'
#' @return Invisibly TRUE if configuration is valid; aborts otherwise.
#' @export
check_config <- function() {
  contact <- Sys.getenv("NRDB_CONTACT", unset = "")
  placeholders <- c("", "changeme", "you@example.com", "TODO")

  if (contact %in% placeholders) {
    rlang::abort(
      "NRDB_CONTACT is missing or still a placeholder; set a real contact address before running any sync.",
      class = "netrunneR_missing_config"
    )
  }

  stopifnot(identical(LLM_USE_POLICY, "no_llm_indexing_of_nrdb_text"))

  invisible(TRUE)
}
