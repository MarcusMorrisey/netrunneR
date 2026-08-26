source(".ci/restore.R")
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")

# Mirrors check.R's own pre-install step (see its comment): build_view_manifest()
# (R/views-matchups.R) calls renv_lock_hash() against pkg-src/renv.lock, which
# only exists once something stages it -- true for an R CMD INSTALL'd package
# (what check.R exercises) but not for devtools::test()'s plain load_all() from
# raw source. No test called compute_ice_breaker_matchups() (the only caller of
# build_view_manifest()) before this branch, so `make test` never needed this
# staging before either.
if (!dir.exists("inst/pkg-src")) {
  dir.create("inst/pkg-src", recursive = TRUE)
  file.copy("R", "inst/pkg-src", recursive = TRUE)
  file.copy("renv.lock", "inst/pkg-src/renv.lock")
}

devtools::test()
