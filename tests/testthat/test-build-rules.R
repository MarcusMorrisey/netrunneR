# Tests build_lineage.netrunneR_web_archive()'s release_id shape and that
# check_version_monotonic()/check_pdf_hashes() are available for its checks list.
test_that("build_lineage.netrunneR_web_archive() writes rules_version rows and a hex-suffixed release_id", {
  raw_dir <- withr::local_tempdir()
  pdf_path <- file.path(raw_dir, "rules.pdf")
  writeBin(charToRaw("pdf-bytes"), pdf_path)

  index <- tibble::tibble(
    version = "6.4", published_date = "2023-01-01", title = "Comprehensive Rules 6.4",
    pdf_url = "https://example.test/rules.pdf", pooled_hash = digest::digest(file = pdf_path, algo = "sha256")
  )

  li <- new_lineage("rules", "web_archive", withr::local_tempdir(), schema_version = 1L,
                    build_module_path = "R/build-rules.R")
  staged_raw <- list(raw_dir = raw_dir, index = index)

  built <- build_lineage.netrunneR_web_archive(li, staged_raw)

  expect_true(fs::file_exists(built$db_path))
  # <UTC timestamp>-<8 hex>, not a bare timestamp: guards against a
  # same-second collision between two rules releases.
  expect_match(built$release_id, "^\\d{8}T\\d{6}Z-[0-9a-f]{8}$")
})

test_that("check_version_monotonic() and check_pdf_hashes() are exposed for build-rules' checks list", {
  expect_true(exists("check_version_monotonic"))
  expect_true(exists("check_pdf_hashes"))
})
