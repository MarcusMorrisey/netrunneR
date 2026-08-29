# Every body below is copied VERBATIM from the mtgred/netrunner tree, not
# paraphrased. A hand-simplified fixture would only prove the parser
# handles the shape someone imagined while writing it, and the shapes it
# actually failed on were the ones nobody imagined.

test_that("a plain two-integer pump still parses", {
  p <- pump_economics('(auto-icebreaker {:abilities [(break-sub 1 1 "Barrier") (strength-pump 1 1)]})')

  expect_true(p$has_pump)
  expect_equal(p$pump_cost, 1L)
  expect_equal(p$pump_amount, 1L)
  expect_true(is.na(p$pump_stealth))
  expect_true(is.na(p$pump_resource_type))
})

test_that("a stealth-qualified credit pump is read, and stays denominated in credits", {
  # Refractor. This is the class of card the old two-integer regex could
  # not see at all, which left it recorded as fixed-strength.
  p <- pump_economics('(strength-pump (->c :credit 1 {:stealth 1}) 3 :end-of-encounter)')

  expect_true(p$has_pump)
  expect_equal(p$pump_cost, 1L)
  expect_equal(p$pump_amount, 3L)
  expect_equal(p$pump_stealth, 1L)
  # A stealth credit is a credit, so nothing here is a non-credit resource.
  expect_true(is.na(p$pump_resource_type))
})

test_that(":all-stealth means every credit of that cost, not one of them", {
  # Afterimage. The qualifier names how many of the credits must be
  # stealth-sourced, and :all-stealth is all of them -- which for a
  # 1-credit cost is 1 and for Blackstone's 3 would be 3.
  p <- pump_economics('(strength-pump (->c :credit 1 {:stealth :all-stealth}) 2 :end-of-encounter)')

  expect_equal(p$pump_cost, 1L)
  expect_equal(p$pump_stealth, 1L)
})

test_that("a fixed stealth count is read as itself, not as the whole cost", {
  # Blackstone pays 3 credits of which only 1 need be stealth. Reading
  # {:stealth 1} as :all-stealth would overstate the constraint threefold.
  p <- pump_economics('(strength-pump (->c :credit 3 {:stealth 1}) 4 :end-of-run)')

  expect_equal(p$pump_cost, 3L)
  expect_equal(p$pump_stealth, 1L)
})

test_that("a counter pump costs no credits and is recorded as a resource", {
  # Propeller. pump_cost is 0 because zero credits really are paid; the
  # power counter is a separate quantity and is deliberately NOT folded
  # into the credit total.
  p <- pump_economics('(strength-pump [(->c :power 1)] 2)')

  expect_true(p$has_pump)
  expect_equal(p$pump_cost, 0L)
  expect_equal(p$pump_amount, 2L)
  expect_equal(p$pump_resource_type, "power")
  expect_equal(p$pump_resource_qty, 1L)
})

test_that("a pump costing BOTH a counter and credits reports both, counted once", {
  # Hantu: [(->c :virus 1) 3] is one virus counter AND three credits. The
  # bare 3 sits beside the ->c form, so it is only found after that form
  # is stripped -- otherwise the virus counter's own 1 would be read as a
  # credit too and the total would come out at 4.
  p <- pump_economics('(strength-pump [(->c :virus 1) 3] 2)')

  expect_equal(p$pump_cost, 3L)
  expect_equal(p$pump_amount, 2L)
  expect_equal(p$pump_resource_type, "virus")
  expect_equal(p$pump_resource_qty, 1L)
})

test_that("the strength gained is read from after the cost, not from inside it", {
  # The regression this guards: taking the first integer anywhere in the
  # form yields the COST's number as the amount. Here the cost's 3 and
  # the amount's 4 are different, so a confusion between them shows up.
  p <- pump_economics('(strength-pump (->c :credit 3 {:stealth 1}) 4 :end-of-run)')

  expect_equal(p$pump_amount, 4L)
})

test_that("a breaker with genuinely no pump is still reported as having none", {
  # Atman. The fix must not turn "no pump" into "some pump" -- that would
  # trade one wrong answer for another.
  p <- pump_economics('(auto-icebreaker {:abilities [(break-sub 1 1 "All")]})')

  expect_false(p$has_pump)
  expect_true(is.na(p$pump_cost))
  expect_true(is.na(p$pump_amount))
})

test_that("breaker_economics reports parsed, not parsed_no_pump, for a stealth breaker", {
  # The end-to-end statement of the bug: parse_status was the thing
  # compute_cost_to_break_formula() trusted when it decided a breaker
  # could not reach ice above its own strength.
  e <- breaker_economics(
    '(auto-icebreaker {:abilities [(break-sub 1 1 "Code Gate") (strength-pump (->c :credit 1 {:stealth 1}) 3 :end-of-encounter)]})'
  )

  expect_equal(e$parse_status, "parsed")
  expect_equal(e$pump_cost, 1L)
  expect_equal(e$pump_stealth, 1L)
})

test_that("a pump is still read when the BREAK cost is the unreadable half", {
  # Switchblade pays a stealth credit to break as well as to pump. The
  # break clause defeats the cost regex, but the pump is independent and
  # dropping it would lose data for no reason.
  e <- breaker_economics(
    '(auto-icebreaker {:abilities [(break-sub (->c :credit 1 {:stealth 1}) 0 "Sentry") (strength-pump (->c :credit 1 {:stealth 1}) 7 :end-of-encounter)]})'
  )

  expect_equal(e$parse_status, "non_credit_break_cost")
  expect_equal(e$pump_cost, 1L)
  expect_equal(e$pump_amount, 7L)
})

# ---- break-clause subtypes ------------------------------------------
# Bodies copied verbatim from the mtgred/netrunner tree, as above.

test_that("the subtype is read positionally, not as the last string in the form", {
  # Endless Hunger recorded a break_subtype of " subroutine" -- the tail of
  # a :msg -- which matches no ice, so the card had no pairings at all.
  # The subtype is the third POSITIONAL argument; cost and quantity are
  # never strings, so the first literal is the subtype and any later one
  # belongs to a label, a message or a :req.
  body <- paste0(
    '(defcard "Endless Hunger" {:abilities [(break-sub [(->c :trash-installed 1)] 1 "All" ',
    '{:msg "break 1 subroutine"})]})')

  expect_equal(break_sub_subtype(body), "All")
})

test_that("a label mentioning the subtype does not displace the real one", {
  body <- '(break-sub 1 1 "Barrier" {:label "Break 1 Barrier subroutine"})'
  expect_equal(break_sub_subtype(body), "Barrier")
})

test_that("break_subtype_count counts DISTINCT subtypes, not clauses", {
  # Odore, BlacKat, Euler and Revolver each carry two clauses naming the
  # same subtype. One record of it is complete, so they must not be
  # flagged as partial -- doing so would suppress a correct definite
  # negative for cards that deserve one.
  body <- '{:abilities [(break-sub 2 0 "Sentry") (break-sub 0 1 "Sentry")]}'
  expect_equal(break_subtype_count(body), 1L)
})

test_that("break_subtype_count sees a card that breaks two different subtypes", {
  # Penrose: Code Gates generally, Barriers only the turn it is installed.
  # Exactly the shape whose second subtype this table drops.
  body <- paste0(
    '{:abilities [(break-sub 1 1 "Barrier" {:req (req (= :this-turn (installed? card)))}) ',
    '(break-sub 1 1 "Code Gate") (strength-pump 1 3)]}')

  expect_equal(break_subtype_count(body), 2L)
})

test_that("break_subtype_count is 0 for a card with no break clause", {
  expect_equal(break_subtype_count('{:abilities [{:label "not a breaker"}]}'), 0L)
})
