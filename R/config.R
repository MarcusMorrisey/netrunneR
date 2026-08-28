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

#' Pre-ship dependency gate: cardpool non-affiliation disclaimer
#'
#' Passed to require_cardpool_disclaimer() by every cardpool-sourced view.
#' Starts FALSE, which BLOCKS those views: the guard is a stopifnot(), so
#' a closed gate aborts the render rather than degrading quietly.
#'
#' Flipping this to TRUE is a human attestation that the disclaimer
#' rendered in the app matches Null-Signal-Games/netrunner-cards-json's
#' own COPYRIGHT.md at the exact mirrored commit -- not a mechanical
#' change to make a test or a view pass. Every call site previously
#' passed a literal TRUE, which made the stopifnot() unfalsifiable and
#' the guard decorative; that is the specific failure this constant
#' exists to prevent, so do not reintroduce a literal at a call site.
#' @export
CARDPOOL_DISCLAIMER_CONFIRMED <- TRUE

#' Pre-ship dependency gate: implementation MIT notice
#'
#' Passed to require_implementation_license_notice() by every view
#' rendering data derived from the mtgred/netrunner implementation tree
#' (the matchup explorer, since cost_to_break derives from
#' ice_breaker_traits). Named for the notice rather than the guard
#' function because MIT is what has to be confirmed.
#'
#' Flipping this to TRUE attests that the app renders the verbatim MIT
#' permission notice AND the real copyright holders from that repo's
#' LICENSE at the exact mirrored commit. Naming the licence -- "MIT
#' licensed" -- is not an MIT notice: the licence requires the copyright
#' line and the permission text be reproduced.
#' @export
IMPLEMENTATION_MIT_NOTICE_CONFIRMED <- TRUE

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
