# Covers validate_release()'s pass/fail aggregation and two validate-helpers checks.
test_that("validate_release() reports status fail when any check fails", {
  li <- new_lineage("cardpool", "git_mirror", "/tmp/unused")
  built <- list(checks = list(list(check = "a", status = "pass", message = "ok")))
  report <- validate_release(li, built, checks = list(list(check = "b", status = "fail", message = "bad")))
  expect_identical(report$status, "fail")
  expect_length(report$checks, 2)
})

test_that("validate_release() reports status pass when all checks pass or warn", {
  li <- new_lineage("cardpool", "git_mirror", "/tmp/unused")
  built <- list(checks = list(list(check = "a", status = "warn", message = "hm")))
  report <- validate_release(li, built)
  expect_identical(report$status, "pass")
})

test_that("check_col_vals_in_set() and check_rows_distinct() flag violations", {
  df <- data.frame(side = c("runner", "corp", "bogus"))
  set_check <- check_col_vals_in_set(df, "side", c("runner", "corp"))
  expect_identical(set_check$status, "fail")

  dup_df <- data.frame(id = c("a", "a", "b"))
  dup_check <- check_rows_distinct(dup_df, "id")
  expect_identical(dup_check$status, "fail")
})
