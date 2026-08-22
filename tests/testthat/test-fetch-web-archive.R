# Tests fetch_lineage.netrunneR_web_archive() against a mocked hub index
# and a mocked not-yet-pooled PDF.
test_that("fetch_lineage.netrunneR_web_archive() parses a fixture hub index and pools a not-yet-pooled PDF", {
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 200, body = charToRaw(
      "<table class='rules-index'><tr><td class='version'>6.6</td><td class='published-date'>2023-01-01</td><td class='title'>Comprehensive Rules 6.6</td><td><a class='pdf-link' href='https://example.test/rules-6.6.pdf'>pdf</a></td></tr></table>"
    )),
    httr2::response(status_code = 200, body = charToRaw("%PDF-1.4 fixture"))
  ))

  li <- new_lineage("rules", "web_archive", withr::local_tempdir(), hub_url = "https://example.test/hub")
  attempt_dir <- withr::local_tempdir()

  staged <- fetch_lineage(li, attempt_dir)

  expect_identical(nrow(staged$index), 1L)
  expect_identical(staged$index$version, "6.6")
  expect_true(nzchar(staged$index$pooled_hash))
  expect_true(fs::file_exists(file.path(
    attempt_dir, "objects", substr(staged$index$pooled_hash, 1, 2), paste0(staged$index$pooled_hash, ".pdf")
  )))
})
