# Canonical fixture for the ice/breaker matchup tests, matching the REAL
# cardpool (inst/sql/schema/cardpool.sql) and implementation
# (inst/sql/schema/implementation.sql) column shapes -- not a separately
# invented sample per test file. Two pairs are named and reused
# everywhere: PAIR_RUNNER_FAVORED (credit_differential > 0) and
# PAIR_CORP_FAVORED (credit_differential < 0).
#
# The implementation side carries ECONOMICS, not subtypes or strengths:
# those come from the cardpool half of the fixture, exactly as they do in
# the real build. Which breaker can touch which ice is decided by the
# breaker's own break_subtype against the ice's printed keywords.
#
# The strengths below are chosen so the formula's branches are all
# reachable by hand:
#
#   ice01 str 1, 1 sub   x brk01 str 1, break 1/1, pump 1/1 -> 0 pump + 1 break = 1
#   ice02 str 3, 2 subs  x brk02 str 3, break 2/1, pump 2/1 -> 0 pump + 4 break = 4
#   ice04 str 4, 2 subs  x brk01 str 1, break 1/1, pump 1/1 -> 3 pump + 2 break = 5
#   ice04 str 4          x brk04 str 2, no pump             -> NA (cannot reach)
#   ice03 subs unknown   x brk03 cost unknown               -> NA (both ends)

PAIR_RUNNER_FAVORED <- list(ice_code = "ice01", breaker_code = "brk01")
PAIR_CORP_FAVORED   <- list(ice_code = "ice02", breaker_code = "brk02")

mini_pool_cardpool <- function() {
  tibble::tribble(
    ~code,   ~title,            ~pack_code, ~faction_code,   ~type_code, ~side_code, ~text, ~cost, ~strength, ~keywords,
    "ice01", "Cheap Wall",      "core",     "neutral-corp",  "ice",      "corp",     "",    2L,    1L,        "Barrier",
    "ice02", "Expensive Code",  "core",     "neutral-corp",  "ice",      "corp",     "",    8L,    3L,        "Code Gate",
    "ice03", "Untouched Gate",  "core",     "neutral-corp",  "ice",      "corp",     "",    4L,    2L,        "Sentry",
    "ice04", "Tall Wall",       "core",     "neutral-corp",  "ice",      "corp",     "",    5L,    4L,        "Barrier",
    "brk01", "Bargain Breaker", "core",     "neutral-runner","program",  "runner",   "",    1L,    1L,        "Icebreaker - Fracter",
    "brk02", "Pricey Breaker",  "core",     "neutral-runner","program",  "runner",   "",    6L,    3L,        "Icebreaker - Decoder",
    "brk03", "Idle Breaker",    "core",     "neutral-runner","program",  "runner",   "",    3L,    2L,        "Icebreaker - Killer",
    "brk04", "Fixed Breaker",   "core",     "neutral-runner","program",  "runner",   "",    4L,    2L,        "Icebreaker - Fracter",
    "brk05", "Stealth Breaker", "core",     "neutral-runner","program",  "runner",   "",    3L,    1L,        "Icebreaker - Fracter",
    "brk06", "Counter Breaker", "core",     "neutral-runner","program",  "runner",   "",    3L,    1L,        "Icebreaker - Fracter"
  )
}

mini_pool_ice_breaker_traits <- function() {
  tibble::tribble(
    ~code,   ~title,            ~kind,     ~subroutine_count, ~break_cost, ~break_qty, ~break_subtype, ~pump_cost, ~pump_amount, ~pump_stealth, ~pump_resource_type, ~pump_resource_qty, ~parse_status,
    "ice01", "Cheap Wall",      "ice",     1L,                NA_integer_, NA_integer_, NA_character_, NA_integer_, NA_integer_, NA_integer_,  NA_character_,       NA_integer_,        "parsed",
    "ice02", "Expensive Code",  "ice",     2L,                NA_integer_, NA_integer_, NA_character_, NA_integer_, NA_integer_, NA_integer_,  NA_character_,       NA_integer_,        "parsed",
    # Subroutine count genuinely varies with game state, as on Ashigaru.
    "ice03", "Untouched Gate",  "ice",     NA_integer_,       NA_integer_, NA_integer_, NA_character_, NA_integer_, NA_integer_, NA_integer_,  NA_character_,       NA_integer_,        "variable_subroutines",
    "ice04", "Tall Wall",       "ice",     2L,                NA_integer_, NA_integer_, NA_character_, NA_integer_, NA_integer_, NA_integer_,  NA_character_,       NA_integer_,        "parsed",
    "brk01", "Bargain Breaker", "program", NA_integer_,       1L,          1L,          "Barrier",     1L,          1L,          NA_integer_,  NA_character_,       NA_integer_,        "parsed",
    "brk02", "Pricey Breaker",  "program", NA_integer_,       2L,          1L,          "Code Gate",   2L,          1L,          NA_integer_,  NA_character_,       NA_integer_,        "parsed",
    # Breaks Sentry, but for a cost the parser will not read as credits --
    # the subtype survives so the pair is still visible as unknown.
    "brk03", "Idle Breaker",    "program", NA_integer_,       NA_integer_, NA_integer_, "Sentry",      NA_integer_, NA_integer_, NA_integer_,  NA_character_,       NA_integer_,        "non_credit_break_cost",
    # A real card design, not a parse failure: fixed strength, no pump.
    "brk04", "Fixed Breaker",   "program", NA_integer_,       1L,          1L,          "Barrier",     NA_integer_, NA_integer_, NA_integer_,  NA_character_,       NA_integer_,        "parsed_no_pump",
    # Pumps with a stealth credit: the credits are ordinary credits and
    # count in pump_cost, and the sourcing constraint rides alongside.
    "brk05", "Stealth Breaker", "program", NA_integer_,       1L,          1L,          "Barrier",     1L,          2L,          1L,           NA_character_,       NA_integer_,        "parsed",
    # Pumps for a power counter and no credits at all. The credit total
    # would be 0, which is true and useless, so the pair stays unknown.
    "brk06", "Counter Breaker", "program", NA_integer_,       1L,          1L,          "Barrier",     0L,          2L,          NA_integer_,  "power",             1L,                 "parsed"
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
