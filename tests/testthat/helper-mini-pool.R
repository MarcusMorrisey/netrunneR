# Canonical fixture for the new ice/breaker matchup tests, matching the
# REAL cardpool (inst/sql/schema/cardpool.sql) and implementation
# (inst/sql/schema/implementation.sql) column shapes -- not a separately
# invented sample per test file. Two pairs are named and reused
# everywhere: PAIR_RUNNER_FAVORED (credit_differential > 0) and
# PAIR_CORP_FAVORED (credit_differential < 0).
#
# subtype_compatible()'s literal-string-overlap check is pre-existing
# behavior, not something this fixture attempts to validate for real
# game accuracy -- "Barrier"/"Code Gate" are used as opaque matching
# tokens on both the ice and breaker side purely so at least one pair is
# subtype-compatible and therefore reaches the cost_to_break/override
# logic under test.

PAIR_RUNNER_FAVORED <- list(ice_code = "ice01", breaker_code = "brk01")
PAIR_CORP_FAVORED   <- list(ice_code = "ice02", breaker_code = "brk02")

mini_pool_cardpool <- function() {
  tibble::tribble(
    ~code,   ~title,            ~pack_code, ~faction_code,   ~type_code, ~side_code, ~text, ~cost, ~strength, ~keywords,
    "ice01", "Cheap Wall",      "core",     "neutral-corp",  "ice",      "corp",     "",    2L,    1L,        "Barrier",
    "ice02", "Expensive Code",  "core",     "neutral-corp",  "ice",      "corp",     "",    8L,    3L,        "Code Gate",
    "ice03", "Untouched Gate",  "core",     "neutral-corp",  "ice",      "corp",     "",    4L,    2L,        "Sentry",
    "brk01", "Bargain Breaker", "core",     "neutral-runner","program",  "runner",   "",    1L,    1L,        "Icebreaker",
    "brk02", "Pricey Breaker",  "core",     "neutral-runner","program",  "runner",   "",    6L,    3L,        "Icebreaker",
    "brk03", "Idle Breaker",    "core",     "neutral-runner","program",  "runner",   "",    3L,    2L,        "Icebreaker"
  )
}

mini_pool_ice_breaker_traits <- function() {
  tibble::tribble(
    ~code,   ~subtypes,    ~base_strength, ~break_cost,
    "ice01", "Barrier",    1L,             NA_integer_,
    "ice02", "Code Gate",  3L,             NA_integer_,
    "ice03", "Sentry",     2L,             NA_integer_,
    "brk01", "Barrier",    1L,             NA_integer_,
    "brk02", "Code Gate",  3L,             NA_integer_,
    "brk03", "Sentry",     2L,             NA_integer_
  )
}

mini_pool_matchup_overrides <- function() {
  # ice01 rez cost 2, override cost_to_break 1 -> differential +1 (runner-favored)
  # ice02 rez cost 8, override cost_to_break 10 -> differential -2 (corp-favored)
  tibble::tribble(
    ~ice_code, ~breaker_code, ~cost_to_break, ~reason, ~verified_by, ~verified_at,
    PAIR_RUNNER_FAVORED$ice_code, PAIR_RUNNER_FAVORED$breaker_code, 1L,
    "fixture", "fixture-author", "2026-01-01T00:00:00Z",
    PAIR_CORP_FAVORED$ice_code, PAIR_CORP_FAVORED$breaker_code, 10L,
    "fixture", "fixture-author", "2026-01-01T00:00:00Z"
  )
}
