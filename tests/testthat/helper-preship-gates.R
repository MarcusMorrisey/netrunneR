# Opening the pre-ship gates for the test suite
#
# The gates in R/config.R default FALSE and abort every guarded view --
# precisely their job, so with them closed no test that instantiates a
# guarded module server can run at all. They are a SHIP gate, not a test
# gate: how a module behaves is what those tests are about, and that is
# unrelated to whether a human has yet attested to the upstream terms.
#
# The shipped values are captured BEFORE opening, and test-preship-gates.R
# asserts against the capture -- so "did someone flip a gate to make a
# view work?" is still a question the suite answers, even though the
# suite itself runs with the gates open.
SHIPPED_GATE_DEFAULTS <- list(
  CARDPOOL_DISCLAIMER_CONFIRMED = netrunneR::CARDPOOL_DISCLAIMER_CONFIRMED,
  IMPLEMENTATION_MIT_NOTICE_CONFIRMED = netrunneR::IMPLEMENTATION_MIT_NOTICE_CONFIRMED,
  RULES_DISCLAIMER_CONFIRMED = netrunneR::RULES_DISCLAIMER_CONFIRMED,
  NRDB_ATTRIBUTION_CONFIRMED = netrunneR::NRDB_ATTRIBUTION_CONFIRMED,
  ABR_ATTRIBUTION_CONFIRMED = netrunneR::ABR_ATTRIBUTION_CONFIRMED
)

local({
  ns <- asNamespace("netrunneR")
  for (gate in names(SHIPPED_GATE_DEFAULTS)) {
    unlockBinding(gate, ns)
    assign(gate, TRUE, envir = ns)
    lockBinding(gate, ns)
  }
})

# Tag output is indented by htmltools, so notice assertions would otherwise
# be matching the renderer's formatting rather than the text.
squish <- function(tag) {
  gsub("[[:space:]]+", " ", paste(as.character(tag), collapse = " "))
}
