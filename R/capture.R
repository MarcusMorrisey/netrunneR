# Security/architecture invariant: capture_response_body() is the ONLY
# function in this package permitted to pull httr2 response bytes onto
# disk. Every fetch_lineage() method must route through it.
#' Return only the body bytes of an httr2 response
#'
#' The sole sanctioned path from an httr2_response object to any byte
#' written to disk anywhere in the package. No fetch_lineage() method may
#' call a disk-write function directly against a response or its headers;
#' every write goes through this function so headers, cookies and the
#' response object itself fall out of scope immediately after the call.
#'
#' @param resp An httr2_response object.
#' @param as Character. "raw" for resp_body_raw(), "string" for resp_body_string().
#'
#' @return A raw vector or a length-1 character vector, per `as`.
#' @export
capture_response_body <- function(resp, as = c("raw", "string")) {
  as <- match.arg(as)
  if (as == "raw") {
    httr2::resp_body_raw(resp)
  } else {
    httr2::resp_body_string(resp)
  }
}
