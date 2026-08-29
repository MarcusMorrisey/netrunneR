# compute_ice_breaker_matchups() had zero test coverage before this file
# (test-views.R only covers compute_identity_ratings()). These tests
# cover the new matchup_overrides input, credit_differential, and the
# source provenance column added alongside it, using the canonical
# mini-pool fixture (helper-mini-pool.R) rather than inventing a
# per-test sample.

test_that("compute_ice_breaker_matchups() requires matchup_overrides -- no default, no silent skip", {
  expect_error(
    compute_ice_breaker_matchups(mini_pool_ice_breaker_traits(), mini_pool_cardpool()),
    regexp = "matchup_overrides"
  )
})

test_that("subtype-incompatible pairs are still excluded (pre-existing behavior preserved)", {
  result <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )
  m <- result$matchups
  # ice01 (Barrier) x brk02 (Code Gate) shares no subtype token -> must not appear at all.
  expect_equal(nrow(m[m$ice_code == "ice01" & m$breaker_code == "brk02", ]), 0)
})

test_that("override rows win, set source == 'override', and the sign convention matches the docs", {
  result <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )
  m <- result$matchups

  runner_row <- m[m$ice_code == PAIR_RUNNER_FAVORED$ice_code & m$breaker_code == PAIR_RUNNER_FAVORED$breaker_code, ]
  expect_equal(runner_row$source, "override")
  expect_equal(runner_row$cost_to_break, 1L)
  expect_equal(runner_row$credit_differential, 1L)   # 2 (rez) - 1 (break) = +1, favors runner
  expect_gt(runner_row$credit_differential, 0)

  corp_row <- m[m$ice_code == PAIR_CORP_FAVORED$ice_code & m$breaker_code == PAIR_CORP_FAVORED$breaker_code, ]
  expect_equal(corp_row$source, "override")
  expect_equal(corp_row$cost_to_break, 10L)
  expect_equal(corp_row$credit_differential, -2L)    # 8 (rez) - 10 (break) = -2, favors corp
  expect_lt(corp_row$credit_differential, 0)
})

test_that("a subtype-compatible pair with no override is honestly not_computable, never a fabricated number", {
  result <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )
  m <- result$matchups
  row <- m[m$ice_code == "ice03" & m$breaker_code == "brk03", ]

  expect_equal(nrow(row), 1)
  expect_equal(row$source, "not_computable")
  expect_true(is.na(row$cost_to_break))
  expect_true(is.na(row$credit_differential))
})

test_that("cost_to_break and credit_differential agree on NA-ness in every row", {
  result <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), mini_pool_matchup_overrides()
  )
  m <- result$matchups
  expect_equal(is.na(m$cost_to_break), is.na(m$credit_differential))
  expect_equal(is.na(m$cost_to_break), m$source == "not_computable")
})

test_that("manifest cache_identity changes with cardpool/implementation release id and overrides content", {
  overrides <- mini_pool_matchup_overrides()

  base <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), overrides,
    cardpool_release_id = "cp1", implementation_release_id = "impl1"
  )
  diff_cardpool_id <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), overrides,
    cardpool_release_id = "cp2", implementation_release_id = "impl1"
  )
  diff_overrides <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), overrides[1, ],
    cardpool_release_id = "cp1", implementation_release_id = "impl1"
  )

  ids <- c(base$manifest$cache_identity, diff_cardpool_id$manifest$cache_identity, diff_overrides$manifest$cache_identity)
  expect_equal(length(unique(ids)), 3)
})

# ---- the packaged overrides file ---------------------------------------

test_that("read_matchup_overrides() types the packaged template, empty as it is", {
  # Regression: the file ships header-only, and readr types every column
  # of a zero-row CSV as character. That made cost_to_break character,
  # which aborted compute_ice_breaker_matchups() inside dplyr::if_else()
  # and so crashed load_ice_breaker_app_data() for every caller -- the
  # app could not start against any real promoted release. Guessed types
  # are the defect, so the assertion is on the declared type, not on the
  # row count that happens to trigger it.
  overrides <- read_matchup_overrides()
  expect_type(overrides$cost_to_break, "integer")
  expect_type(overrides$ice_code, "character")
})

test_that("compute_ice_breaker_matchups() survives the packaged empty overrides", {
  matchups <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), read_matchup_overrides()
  )$matchups

  expect_true(nrow(matchups) > 0)
  expect_type(matchups$cost_to_break, "integer")
  # With no override rows, nothing can report source "override".
  expect_false(any(matchups$source == "override"))
})

test_that("a populated overrides file still types cost_to_break as integer", {
  # The complement: with rows present readr would have guessed integer
  # and appeared to work, which is why the empty case was the one that
  # bit. Both paths must land on the same declared type.
  path <- withr::local_tempfile(fileext = ".csv")
  writeLines(c(
    "ice_code,breaker_code,cost_to_break,reason,verified_by,verified_at",
    "ice01,brk01,4,fixture,fixture-author,2026-01-01T00:00:00Z"
  ), path)

  overrides <- read_matchup_overrides(path)
  expect_type(overrides$cost_to_break, "integer")
  expect_equal(overrides$cost_to_break, 4L)
})

# ---- the app's card pool -----------------------------------------------

pool_fixture <- function() {
  tibble::tribble(
    ~code,   ~title,          ~type_code, ~keywords,
    "ice01", "Some ICE",      "ice",      "Barrier",
    "brk01", "A Breaker",     "program",  "Icebreaker - Fracter",
    "brk02", "AI Breaker",    "program",  "Icebreaker - AI",
    "prg01", "Datasucker-ish","program",  "Virus",
    "prg02", "No Subtypes",   "program",  NA_character_,
    "hw01",  "Boomerang-ish", "hardware", "Trash",
    "res01", "A Resource",    "resource", "Connection"
  )
}

test_that("ice_breaker_pool() keeps ICE and icebreaker programs only", {
  kept <- ice_breaker_pool(pool_fixture())
  expect_equal(sort(kept$code), c("brk01", "brk02", "ice01"))
})

test_that("ice_breaker_pool() drops programs that are not icebreakers", {
  # The initial-release scope decision: every card that breaks, bypasses
  # or interacts with ICE is the eventual intent, but a non-breaker
  # program is not comparable on the cost-to-break axis.
  kept <- ice_breaker_pool(pool_fixture())
  expect_false("prg01" %in% kept$code)
  expect_false("prg02" %in% kept$code)
  expect_false("hw01" %in% kept$code)
})

test_that("has_card_subtype() matches whole tokens, not substrings", {
  expect_true(has_card_subtype("Icebreaker - Fracter", "Icebreaker"))
  expect_true(has_card_subtype("AI - Icebreaker", "Icebreaker"))
  expect_false(has_card_subtype("Icebreaker Support", "Icebreaker"))
  expect_false(has_card_subtype(NA_character_, "Icebreaker"))
  expect_identical(
    has_card_subtype(c("Barrier", "Icebreaker - AI", NA), "Icebreaker"),
    c(FALSE, TRUE, FALSE)
  )
})

test_that("has_card_subtype() handles an empty card set", {
  # An empty cardpool/traits join is legitimate -- e.g. while the
  # implementation lineage still keys traits by something the cardpool
  # does not carry. ifelse() collapsed character(0) to logical(0), which
  # strsplit() then rejected, crashing the app at startup.
  expect_identical(has_card_subtype(character(0), "Icebreaker"), logical(0))
})

test_that("compute_ice_breaker_matchups() survives an empty join", {
  traits <- tibble::tibble(
    code = character(0), title = character(0), kind = character(0),
    subroutine_count = integer(0), break_cost = integer(0),
    break_qty = integer(0), break_subtype = character(0),
    pump_cost = integer(0), pump_amount = integer(0),
    pump_stealth = integer(0), pump_resource_type = character(0),
    pump_resource_qty = integer(0),
    parse_status = character(0)
  )
  cards <- tibble::tibble(
    code = character(0), type_code = character(0), keywords = character(0),
    cost = integer(0), strength = integer(0)
  )
  overrides <- tibble::tibble(
    ice_code = character(0), breaker_code = character(0),
    cost_to_break = numeric(0)
  )
  result <- compute_ice_breaker_matchups(traits, cards, overrides)
  expect_identical(nrow(result$matchups), 0L)
})

test_that("compute_ice_breaker_matchups() pairs ICE with icebreakers only", {
  # Previously every program entered the cross-join, so a piece of ICE was
  # paired with Datasucker and friends.
  traits <- tibble::tribble(
    ~code,   ~title,      ~kind,     ~subroutine_count, ~break_cost, ~break_qty, ~break_subtype, ~pump_cost, ~pump_amount, ~pump_stealth, ~pump_resource_type, ~pump_resource_qty, ~parse_status,
    "ice01", "Some ICE",  "ice",     1L,                NA_integer_, NA_integer_, NA_character_, NA_integer_, NA_integer_, NA_integer_,  NA_character_,       NA_integer_,        "parsed",
    "brk01", "A Breaker", "program", NA_integer_,       1L,          1L,          "Barrier",     1L,          1L,          NA_integer_,  NA_character_,       NA_integer_,        "parsed",
    # A virus program: it has no break clause at all, so the real build
    # would never emit a row for it. One is written here anyway, to prove
    # the Icebreaker keyword check is what excludes it.
    "prg01", "A Virus",   "program", NA_integer_,       1L,          1L,          "Barrier",     1L,          1L,          NA_integer_,  NA_character_,       NA_integer_,        "parsed"
  )
  cardpool <- tibble::tribble(
    ~code,   ~title,      ~type_code, ~keywords,              ~cost, ~strength,
    "ice01", "Some ICE",  "ice",      "Barrier",              2L,    1L,
    "brk01", "A Breaker", "program",  "Icebreaker - Fracter", 1L,    1L,
    "prg01", "A Virus",   "program",  "Virus",                1L,    1L
  )

  matchups <- compute_ice_breaker_matchups(
    traits, cardpool, mini_pool_matchup_overrides()
  )$matchups

  expect_true(all(matchups$breaker_code == "brk01"))
  expect_false("prg01" %in% matchups$breaker_code)
})

# ---- the cost-to-break formula ---------------------------------------
# Fixture arithmetic, worked by hand in helper-mini-pool.R. Each case
# names the branch it covers, because a single wrong number here is the
# difference between a comparison tool and a plausible-looking one.

matchup_cost <- function(ice_code, breaker_code) {
  m <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(),
    mini_pool_matchup_overrides()[0, ]
  )$matchups
  row <- m[m$ice_code == ice_code & m$breaker_code == breaker_code, ]
  if (nrow(row) == 0) return(NULL)
  row
}

test_that("a breaker already at strength pays only to break", {
  # ice01 str 1, 1 sub; brk01 str 1, break 1/1 -> 0 pump + 1 break = 1
  row <- matchup_cost("ice01", "brk01")
  expect_equal(row$cost_to_break, 1L)
  expect_equal(row$source, "formula")
})

test_that("multiple subroutines are broken one application each", {
  # ice02 str 3, 2 subs; brk02 str 3, break 2/1 -> 0 pump + 2 x 2 = 4
  expect_equal(matchup_cost("ice02", "brk02")$cost_to_break, 4L)
})

test_that("a strength gap is pumped before breaking, and both are charged", {
  # ice04 str 4, 2 subs; brk01 str 1, pump 1/1, break 1/1
  # -> 3 pumps x 1 + 2 breaks x 1 = 5
  expect_equal(matchup_cost("ice04", "brk01")$cost_to_break, 5L)
})

test_that("a breaker that cannot reach the ice's strength gets NA, not a big number", {
  # brk04 has no strength-pump and is str 2 against ice04's str 4. "You
  # cannot do this" and "this is expensive" are different answers, and
  # only one of them is true.
  row <- matchup_cost("ice04", "brk04")
  expect_true(is.na(row$cost_to_break))
  expect_equal(row$source, "not_computable")
})

test_that("that same pumpless breaker still costs a normal break within its strength", {
  # ice01 is str 1, below brk04's str 2, so no pump is needed and none is
  # charged -- the missing pump columns only matter when there is a gap.
  expect_equal(matchup_cost("ice01", "brk04")$cost_to_break, 1L)
})

test_that("an ice with a variable subroutine count is not computable", {
  row <- matchup_cost("ice03", "brk03")
  expect_true(is.na(row$cost_to_break))
  expect_equal(row$source, "not_computable")
})

test_that("a breaker whose break cost is not credits still appears, as unknown", {
  # brk03 declares Sentry but its cost defeated the parser. The pair must
  # exist: a pair that is absent and a pair that is unknown look identical
  # to a reader, and only one of them is true.
  expect_false(is.null(matchup_cost("ice03", "brk03")))
})

test_that("pairing follows the breaker's declared subtype, not shared keywords", {
  # brk02 breaks Code Gate, so it never meets a Barrier however many
  # keywords the two cards happen to share.
  expect_null(matchup_cost("ice01", "brk02"))
  expect_false(is.null(matchup_cost("ice02", "brk02")))
})

test_that("an AI breaker's 'All' subtype meets every ice", {
  traits <- mini_pool_ice_breaker_traits()
  traits$break_subtype[traits$code == "brk01"] <- "All"
  m <- compute_ice_breaker_matchups(
    traits, mini_pool_cardpool(), mini_pool_matchup_overrides()[0, ]
  )$matchups
  reached <- sort(unique(m$ice_code[m$breaker_code == "brk01"]))
  expect_equal(reached, c("ice01", "ice02", "ice03", "ice04"))
})

test_that("break_qty 0 means one application for all subroutines", {
  # How this codebase writes Begemot: one payment, whatever the count.
  traits <- mini_pool_ice_breaker_traits()
  traits$break_qty[traits$code == "brk01"] <- 0L
  m <- compute_ice_breaker_matchups(
    traits, mini_pool_cardpool(), mini_pool_matchup_overrides()[0, ]
  )$matchups
  # ice04: 2 subs, but one application -> 3 pumps x 1 + 1 x 1 = 4
  expect_equal(m$cost_to_break[m$ice_code == "ice04" & m$breaker_code == "brk01"], 4L)
})

test_that("the formula computes without warnings", {
  # x/0 is Inf and as.integer(Inf) warns while quietly producing NA, so a
  # divisor guard written inside ifelse() does not actually prevent it.
  expect_no_warning(
    compute_ice_breaker_matchups(
      mini_pool_ice_breaker_traits(), mini_pool_cardpool(),
      mini_pool_matchup_overrides()
    )
  )
})

# ---- stealth and non-credit pump costs -------------------------------
# Arithmetic worked by hand against helper-mini-pool.R, as above.
# brk05 "Stealth Breaker": strength 1, break 1/1, pump 1 credit for +2,
#   of which 1 must be stealth.
# brk06 "Counter Breaker": strength 1, break 1/1, pump 0 credits and one
#   power counter for +2.
# ice01 "Cheap Wall": strength 1, 1 subroutine, rez 2.
# ice04 "Tall Wall":  strength 4, 2 subroutines, rez 5.

test_that("a stealth pump is priced in credits like any other pump", {
  # gap 4-1 = 3, pump +2 -> 2 applications at 1 credit = 2
  # break 2 subroutines at 1 each = 2
  row <- matchup_cost("ice04", "brk05")

  expect_equal(row$source, "formula")
  expect_equal(row$cost_to_break, 4L)
  expect_equal(row$credit_differential, 1L)
})

test_that("stealth_credits says how many of those credits must be stealth", {
  # 2 pump applications, 1 stealth credit each. It is a SUBSET of
  # cost_to_break, never an addition to it -- 4 credits are paid, and 2 of
  # them cannot come from the general pool.
  row <- matchup_cost("ice04", "brk05")

  expect_equal(row$stealth_credits, 2L)
  expect_lte(row$stealth_credits, row$cost_to_break)
})

test_that("stealth_credits is NA, not 0, when the pump is never used", {
  # brk05 already matches ice01's strength, so no pump is applied and no
  # stealth credit is required. NA rather than 0 because this column is
  # about a requirement that does not exist here, and 0 would read as a
  # computed quantity.
  row <- matchup_cost("ice01", "brk05")

  expect_equal(row$cost_to_break, 1L)
  expect_true(is.na(row$stealth_credits))
})

test_that("a pump costing a power counter has no credit answer when it is needed", {
  # The credit total would be 2 -- zero for two pump applications, plus 2
  # to break -- which is arithmetically true and thoroughly misleading,
  # because it omits the two power counters entirely. Unknown is the
  # honest answer until the counters are shown alongside.
  row <- matchup_cost("ice04", "brk06")

  expect_equal(row$source, "not_computable")
  expect_true(is.na(row$cost_to_break))
  expect_true(is.na(row$credit_differential))
})

test_that("the same counter breaker IS computable against ice it need not pump for", {
  # The non-credit cost only defeats the arithmetic where the pump is
  # actually used. Against ice at or below its own strength, brk06 pays
  # its break cost in plain credits and nothing else.
  row <- matchup_cost("ice01", "brk06")

  expect_equal(row$source, "formula")
  expect_equal(row$cost_to_break, 1L)
})

test_that("an override drops the formula's stealth count rather than keeping it", {
  # An override replaces the cost outright, and it may have priced the
  # encounter with a different number of pump applications than the
  # formula assumed. Carrying the formula's stealth count alongside
  # someone else's total would describe a line of play nobody chose.
  overrides <- tibble::tibble(
    ice_code = "ice04", breaker_code = "brk05", cost_to_break = 99L
  )
  m <- compute_ice_breaker_matchups(
    mini_pool_ice_breaker_traits(), mini_pool_cardpool(), overrides
  )$matchups
  row <- m[m$ice_code == "ice04" & m$breaker_code == "brk05", ]

  expect_equal(row$source, "override")
  expect_equal(row$cost_to_break, 99L)
  expect_true(is.na(row$stealth_credits))
})

test_that("a release predating the pump-cost columns still computes, rather than aborting", {
  # The sync image is built from a pinned package SHA, so the store can
  # hold a release built before these columns existed while the app
  # reading it already knows about them. That must degrade to "not
  # known", not abort the whole app at startup -- which is exactly what
  # selecting a missing column does.
  old_traits <- mini_pool_ice_breaker_traits()
  old_traits$pump_stealth <- NULL
  old_traits$pump_resource_type <- NULL
  old_traits$pump_resource_qty <- NULL

  m <- compute_ice_breaker_matchups(
    old_traits, mini_pool_cardpool(), mini_pool_matchup_overrides()[0, ]
  )$matchups

  expect_gt(nrow(m), 0)
  # Every stealth requirement is unknown rather than asserted as absent.
  expect_true(all(is.na(m$stealth_credits)))
  # And the ordinary pairs still price exactly as they did before.
  row <- m[m$ice_code == "ice01" & m$breaker_code == "brk01", ]
  expect_equal(row$source, "formula")
})
