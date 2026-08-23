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
  # raw_dir is nested one level under attempt_dir (attempt_dir/raw), same
  # as every other lineage's fetch step -- confirmed live this session:
  # returning attempt_dir itself here made build_lineage.netrunneR_web_archive()'s
  # file.path(raw_dir, "..", "processed", "rules.sqlite") resolve to
  # staging/processed/rules.sqlite, a single path SHARED across every
  # attempt rather than scoped to one, so a second real run always
  # collided on the first's leftover "table rules_version already
  # exists" and could never succeed.
  raw_dir <- file.path(attempt_dir, "raw")
  fs::dir_create(raw_dir)

  index_req <- httr2::request(lineage$hub_url)
  index_resp <- httr2::req_perform(index_req)
  index_html <- rvest::read_html(capture_response_body(index_resp, as = "string"))

  index <- parse_rules_hub_index(index_html)

  objects_dir <- file.path(raw_dir, "objects")
  fs::dir_create(objects_dir)

  index$pooled_hash <- purrr::map_chr(index$pdf_url, function(url) pool_pdf(url, objects_dir))

  list(
    raw_dir = raw_dir,
    index = index,
    content_identity = digest::digest(index$pooled_hash, algo = "sha256")
  )
}

#' Parse the rules hub index HTML into a version/date/title/url tibble
#'
#' The hub page (https://nullsignal.games/rules/comp-rules/) has no
#' `<table>` markup at all: the latest release sits in a Kadence
#' info-box block, and every older release is a plain `<p><a>` link
#' further down the same `.entry-content` region. There is no published
#' date anywhere in the page text or markup, so `published_date` is
#' sourced from each candidate PDF's own `Last-Modified` response
#' header via a HEAD request.
#'
#' Candidates are identified by link text: text must contain
#' "Comprehensive Rules" case-insensitively, and must not contain "Card
#' Text Updates" (the separate card-text-update category). The
#' changes-highlighted variant of the latest PDF describes itself in
#' link text as "PDF with Changes Highlighted" rather than as
#' "Annotated", so the "Annotated" exclusion is checked against the
#' href (which does carry an `_Annotated` filename suffix), not the
#' text. Each surviving href is deduplicated, since the hub links the
#' same PDF twice for several releases (a heading link plus a
#' "Download" button).
#'
#' @param html An xml_document parsed by rvest::read_html().
#' @return A tibble with columns `version`, `published_date`, `title`,
#'   `pdf_url`.
#' @keywords internal
parse_rules_hub_index <- function(html) {
  links <- rvest::html_elements(html, ".entry-content a[href$='.pdf']")
  href <- rvest::html_attr(links, "href")
  text <- stringr::str_squish(rvest::html_text(links))

  is_candidate <- stringr::str_detect(text, stringr::regex("comprehensive rules", ignore_case = TRUE)) &
    !stringr::str_detect(text, stringr::regex("card text updates", ignore_case = TRUE)) &
    !stringr::str_detect(href, stringr::regex("annotated", ignore_case = TRUE))

  href <- href[is_candidate]
  text <- text[is_candidate]

  not_dup <- !duplicated(href)
  href <- href[not_dup]
  text <- text[not_dup]

  version <- stringr::str_match(text, stringr::regex("v\\.?(\\d+\\.\\d+)", ignore_case = TRUE))[, 2]

  published_date <- purrr::map(href, head_last_modified)

  tibble::tibble(
    version = version,
    published_date = do.call(c, published_date),
    title = text,
    pdf_url = href
  )
}

#' HEAD a candidate PDF URL and return its Last-Modified date
#'
#' The hub page carries no published-date text anywhere, so this is the
#' only source of `published_date` for the parsed index. `resp_date()`
#' parses httr2's own `Date` response header, not `Last-Modified`, so
#' the header is read and parsed directly here instead.
#'
#' @param url Character. PDF URL to HEAD.
#' @return A length-1 Date.
#' @keywords internal
head_last_modified <- function(url) {
  resp <- httr2::req_perform(httr2::req_method(httr2::request(url), "HEAD"))
  last_modified <- httr2::resp_header(resp, "last-modified")
  as.Date(as.POSIXct(last_modified, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT"))
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
