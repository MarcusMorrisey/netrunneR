# Enforces, by static AST scan and by a behavioral sentinel-header run,
# the package-wide invariant that capture_response_body() (R/capture.R)
# is the only function permitted to write httr2 response bytes to disk;
# no fetch_lineage() method may write a response or its headers directly.
test_that("no fetch module writes an httr2_response or resp_headers() result to disk", {
  pkg_root <- system.file(package = "netrunneR")
  r_dir <- if (nzchar(pkg_root)) file.path(pkg_root, "R") else testthat::test_path("..", "..", "R")
  skip_if_not(fs::dir_exists(r_dir), "R/ source not resolvable from installed package")

  fetch_files <- fs::dir_ls(r_dir, glob = "*fetch*.R")
  write_fns <- c("writeBin", "writeLines", "write_json", "saveRDS", "dbWriteTable")

  # Walks each candidate call's actual argument expressions rather than
  # flattening the whole top-level expression with all.names(): a match
  # requires a write-fn argument to literally be the resp symbol or a
  # resp_headers() call, not mere co-occurrence anywhere in the expression.
  arg_is_response_like <- function(e) {
    if (is.symbol(e)) return(identical(as.character(e), "resp"))
    if (is.call(e)) {
      head <- tryCatch(as.character(e[[1]])[1], error = function(...) NA_character_)
      if (identical(head, "resp_headers")) return(TRUE)
      return(any(vapply(as.list(e)[-1], arg_is_response_like, logical(1))))
    }
    FALSE
  }

  find_offending_calls <- function(e) {
    hit <- FALSE
    if (is.call(e)) {
      head <- tryCatch(as.character(e[[1]])[1], error = function(...) NA_character_)
      if (!is.na(head) && head %in% write_fns) {
        args <- as.list(e)[-1]
        if (any(vapply(args, arg_is_response_like, logical(1)))) hit <- TRUE
      }
      for (part in as.list(e)) {
        if (is.call(part) && isTRUE(find_offending_calls(part))) hit <- TRUE
      }
    }
    hit
  }

  offenders <- character(0)
  for (f in fetch_files) {
    exprs <- tryCatch(parse(f, keep.source = FALSE), error = function(e) NULL)
    if (is.null(exprs)) next
    if (any(vapply(as.list(exprs), find_offending_calls, logical(1)))) {
      offenders <- c(offenders, f)
    }
  }
  expect_identical(offenders, character(0))
})

test_that("a Set-Cookie sentinel never reaches raw, objects, manifest.json or releases.jsonl", {
  sentinel <- "SENTINEL-HEADER-LEAK-TOKEN"

  httr2::local_mocked_responses(function(req) {
    body <- if (grepl("/entries", req$url) || grepl("/videos$", req$url) || grepl("/upcoming$", req$url)) {
      list()
    } else {
      list(tournament_count = 0L, results = list())
    }
    httr2::response(
      status_code = 200,
      headers = list(`Set-Cookie` = paste0("sid=", sentinel)),
      body = charToRaw(jsonlite::toJSON(body, auto_unbox = TRUE))
    )
  })

  store_root <- withr::local_tempdir()
  li <- new_lineage("abr", "api_poll", store_root, base_url = "https://example.test/api",
                    schema_version = 1L, build_module_path = "R/build-abr.R")

  # A full sync (fetch -> build -> validate -> stage -> promote) populates
  # raw/, manifest.json and releases.jsonl; run_abr_backfill() is the only
  # path that writes objects/, so it is driven separately with one
  # tournament id to bring that path under the scan too.
  run_sync(li, mode = "backfill")
  run_abr_backfill(li, tournament_ids = "999")

  written <- fs::dir_ls(store_root, recurse = TRUE, type = "file")
  hits <- character(0)
  for (f in written) {
    # readLines() on a binary file (e.g. the processed .sqlite database)
    # either errors on embedded NUL bytes or splits the sentinel across
    # two "lines", so bytes are read raw and matched as a byte sequence.
    bytes <- readBin(f, "raw", n = fs::file_size(f))
    if (length(grepRaw(charToRaw(sentinel), bytes, fixed = TRUE)) > 0) {
      hits <- c(hits, f)
    }
  }
  expect_identical(hits, character(0))
})
