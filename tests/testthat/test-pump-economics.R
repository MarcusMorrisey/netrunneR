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
