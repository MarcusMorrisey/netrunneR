#' Fetch the Comprehensive Rules hub index and pool each PDF
#'
#' Requests the Comprehensive Rules hub index with httr2, parses it with
#' rvest into a tibble of version, published_date, title and pdf_url, and
#' downloads each not-yet-pooled PDF through capture_response_body() into
#' objects/<sha256 prefix>/<sha256>.pdf.
#'
#' @param lineage A lineage object of class netrunneR_web_archive.
#' @param attempt_dir Character. Staging directory for this sync attempt.
#' @param ... Ignored.
#'
#' @return A list with `raw_dir`, `index`, and `content_identity`.
#' @export
fetch_lineage.netrunneR_web_archive <- function(lineage, attempt_dir, ...) {
  index_req <- httr2::request(lineage$hub_url)
  index_resp <- httr2::req_perform(index_req)
  index_html <- rvest::read_html(capture_response_body(index_resp, as = "string"))

  index <- parse_rules_hub_index(index_html)

  objects_dir <- file.path(attempt_dir, "objects")
  fs::dir_create(objects_dir)

  index$pooled_hash <- purrr::map_chr(index$pdf_url, function(url) pool_pdf(url, objects_dir))

  list(
    raw_dir = attempt_dir,
    index = index,
    content_identity = digest::digest(index$pooled_hash, algo = "sha256")
  )
}

#' Parse the rules hub index HTML into a version/date/title/url tibble
#' @param html An xml_document parsed by rvest::read_html().
#' @return A tibble with columns `version`, `published_date`, `title`,
#'   `pdf_url`.
#' @keywords internal
parse_rules_hub_index <- function(html) {
  rows <- rvest::html_elements(html, "table.rules-index tr")
  tibble::tibble(
    version = rvest::html_text(rvest::html_elements(rows, ".version")),
    published_date = as.Date(rvest::html_text(rvest::html_elements(rows, ".published-date"))),
    title = rvest::html_text(rvest::html_elements(rows, ".title")),
    pdf_url = rvest::html_attr(rvest::html_elements(rows, "a.pdf-link"), "href")
  )
}

#' Download a not-yet-pooled PDF into the content-addressed object pool
#' @param url Character. PDF URL to fetch.
#' @param objects_dir Character. Root of the content-addressed object pool.
#' @return Character. The sha256 hash of the PDF's bytes.
#' @keywords internal
pool_pdf <- function(url, objects_dir) {
  resp <- httr2::req_perform(httr2::request(url))
  body <- capture_response_body(resp, as = "raw")
  sha <- digest::digest(body, algo = "sha256", serialize = FALSE)

  prefix_dir <- file.path(objects_dir, substr(sha, 1, 2))
  fs::dir_create(prefix_dir)
  object_path <- file.path(prefix_dir, paste0(sha, ".pdf"))
  if (!fs::file_exists(object_path)) writeBin(body, object_path)

  sha
}
