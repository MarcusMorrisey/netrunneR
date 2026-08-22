# Shared test fixtures / helpers loaded before every test file in this
# package's testthat suite.
# Supplies a non-placeholder NRDB_CONTACT so check_config() passes during
# tests without requiring a real operator contact address.
withr::local_envvar(NRDB_CONTACT = "test@example.com", .local_envir = teardown_env())
