# Tests check_config()'s startup validation, including the LLM_USE_POLICY
# and LLM_USE_POLICY_PRECAUTIONARY assertions (see R/config.R).
test_that("check_config() passes with a non-placeholder NRDB_CONTACT", {
  withr::local_envvar(NRDB_CONTACT = "test@example.com")
  expect_true(check_config())
})

test_that("check_config() aborts on a missing or placeholder NRDB_CONTACT", {
  withr::local_envvar(NRDB_CONTACT = "")
  expect_error(check_config(), class = "netrunneR_missing_config")

  withr::local_envvar(NRDB_CONTACT = "changeme")
  expect_error(check_config(), class = "netrunneR_missing_config")
})

test_that("check_config() asserts both LLM_USE_POLICY and LLM_USE_POLICY_PRECAUTIONARY", {
  withr::local_envvar(NRDB_CONTACT = "test@example.com")
  expect_identical(LLM_USE_POLICY, "no_llm_indexing_of_nrdb_text")
  expect_identical(LLM_USE_POLICY_PRECAUTIONARY, "no_llm_indexing_of_cardpool_implementation_rules_text")
  expect_true(check_config())
})
