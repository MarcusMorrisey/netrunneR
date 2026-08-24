# Package-wide runtime policy constants and startup configuration checks.
#' Package-wide LLM use policy
#'
#' Any code path that would index nrdb-sourced free text for an LLM must
#' assert this constant with stopifnot() rather than relying on a comment,
#' so the constraint fails loudly if that code path is ever reached before
#' the policy is revisited.
#'
#' This constant is grounded in NetrunnerDB's own stated terms -- it is a
#' real, verified, source-specific requirement. Contrast
#' LLM_USE_POLICY_PRECAUTIONARY below, which extends the same restriction
#' to other sources as a conservative default, not because those sources
#' state the same requirement.
#' @export
LLM_USE_POLICY <- "no_llm_indexing_of_nrdb_text"

#' Precautionary LLM use policy for cardpool, implementation, and rules text
#'
#' No stated AI/LLM/training restriction was found for the cardpool
#' (Null-Signal-Games/netrunner-cards-json), implementation
#' (mtgred/netrunner), or rules (nullsignal.games Comprehensive Rules)
#' sources -- their READMEs, COPYRIGHT.md, and license text were checked
#' and none states an LLM or training restriction. This constant applies
#' the same no-LLM-indexing precaution to those sources anyway, as a
#' conservative default given they are all unofficial fan content
#' adjacent to the same Fantasy Flight Games / Wizards of the Coast IP as
#' NetrunnerDB.
#'
#' Unlike LLM_USE_POLICY above, this is NOT grounded in a confirmed
#' per-source requirement -- any code path that would index
#' cardpool-, implementation-, or rules-sourced free text for an LLM
#' must assert this constant with stopifnot() rather than relying on a
#' comment, so the precaution fails loudly if that code path is ever
#' reached before it is revisited, but it should not be mistaken for
#' having the same evidentiary weight as LLM_USE_POLICY.
#' @export
LLM_USE_POLICY_PRECAUTIONARY <- "no_llm_indexing_of_cardpool_implementation_rules_text"

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
  stopifnot(identical(LLM_USE_POLICY_PRECAUTIONARY, "no_llm_indexing_of_cardpool_implementation_rules_text"))

  invisible(TRUE)
}
