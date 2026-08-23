# Tests fetch_lineage.netrunneR_web_archive() against a fixture hub index
# modeled on the real page structure (a Kadence info-box for the latest
# release plus plain <p><a> links for older releases, with a Card Text
# Updates section and an _Annotated variant that must both be excluded),
# and mocked not-yet-pooled PDFs.
test_that("fetch_lineage.netrunneR_web_archive() parses a fixture hub index and pools not-yet-pooled PDFs", {
  httr2::local_mocked_responses(list(
    # GET the hub index.
    httr2::response(status_code = 200, body = charToRaw(paste0(
      "<div class='entry-content'>",
      "<div class='wp-block-kadence-infobox'>",
      "<a href='https://example.test/Rules_v6.6.pdf'>Comprehensive Rules v6.6 (PDF)</a>",
      "<a href='https://example.test/Rules_v6.6_Annotated.pdf'>Comprehensive Rules v6.6 (PDF with Changes Highlighted)</a>",
      "<a href='https://example.test/comp-rules-hub'>Comprehensive Rules v6.6 (Web)</a>",
      "</div>",
      "<p><a href='https://example.test/rules-1.6.pdf'>Netrunner Comprehensive Rules v1.6</a></p>",
      "<p><a href='https://example.test/card-text-v6.6.pdf'>Netrunner Card Text Updates v6.6</a></p>",
      "</div>"
    ))),
    # HEAD https://example.test/Rules_v6.6.pdf
    httr2::response(status_code = 200, headers = list(`Last-Modified` = "Mon, 02 Mar 2026 00:18:05 GMT")),
    # HEAD https://example.test/rules-1.6.pdf
    httr2::response(status_code = 200, headers = list(`Last-Modified` = "Tue, 12 Oct 2021 00:00:00 GMT")),
    # GET https://example.test/Rules_v6.6.pdf
    httr2::response(status_code = 200, body = charToRaw("%PDF-1.4 fixture v6.6")),
    # GET https://example.test/rules-1.6.pdf
    httr2::response(status_code = 200, body = charToRaw("%PDF-1.4 fixture v1.6"))
  ))

  li <- new_lineage("rules", "web_archive", withr::local_tempdir(), hub_url = "https://example.test/hub")
  attempt_dir <- withr::local_tempdir()

  staged <- fetch_lineage(li, attempt_dir)

  expect_identical(nrow(staged$index), 2L)
  expect_identical(staged$index$version, c("6.6", "1.6"))
  expect_identical(staged$index$published_date, as.Date(c("2026-03-02", "2021-10-12")))
  expect_identical(staged$index$pdf_url, c(
    "https://example.test/Rules_v6.6.pdf",
    "https://example.test/rules-1.6.pdf"
  ))
  expect_true(all(nzchar(staged$index$pooled_hash)))
  for (sha in staged$index$pooled_hash) {
    expect_true(fs::file_exists(file.path(
      attempt_dir, "objects", substr(sha, 1, 2), paste0(sha, ".pdf")
    )))
  }
})
