#' A minimal reader for the mtgred/netrunner Clojure card definitions
#'
#' NOT a Clojure reader. It understands exactly three things: balanced
#' `()`/`[]`/`{}`, string literals, and backslash escapes inside them --
#' enough to slice one top-level `(defcard "Title" ...)` out of a file
#' and to count the elements of a vector literal. Everything past that is
#' handled by targeted regexes over the sliced body, so a form this
#' cannot read produces NA rather than a guess.
#'
#' Written rather than taken from a package because there is no Clojure
#' reader on CRAN, and the alternative -- running a JVM to evaluate the
#' definitions -- would make an offline mirror depend on being able to
#' build somebody else's Clojure project.
#'
#' @name parse-clojure
#' @keywords internal
NULL

#' Characters Clojure treats as whitespace
#'
#' The comma is in here because Clojure reads it as whitespace, so
#' `[a, b]` is a two-element vector and counting it as three would be
#' wrong.
#' @keywords internal
CLOJURE_WHITESPACE <- c(" ", "\n", "\t", "\r", ",")

#' Find the index that closes the form opening at `start`
#'
#' Operates on a pre-split character vector rather than a string:
#' `substr()` in a loop over a 200,000-character file is quadratic and
#' takes minutes, which is the difference between this running inside a
#' build and not.
#'
#' @param ch Character vector, one element per character.
#' @param start Integer index of an opening delimiter.
#' @return Integer index of the matching close, or `NA_integer_` if the
#'   form never closes.
#' @keywords internal
read_form <- function(ch, start) {
  depth <- 0L
  in_string <- FALSE
  i <- start
  n <- length(ch)
  while (i <= n) {
    c1 <- ch[i]
    if (in_string) {
      if (c1 == "\\") i <- i + 1L else if (c1 == '"') in_string <- FALSE
    } else if (c1 == '"') {
      in_string <- TRUE
    } else if (c1 == "(" || c1 == "[" || c1 == "{") {
      depth <- depth + 1L
    } else if (c1 == ")" || c1 == "]" || c1 == "}") {
      depth <- depth - 1L
      if (depth == 0L) return(i)
    }
    i <- i + 1L
  }
  NA_integer_
}

#' Every `(defcard "Title" ...)` form in one file
#'
#' @param path Character. Path to a `.clj` card-definition file.
#' @return A list of `list(title, body)`, body being the whole form as
#'   text.
#' @keywords internal
read_defcards <- function(path) {
  text <- paste(readLines(path, warn = FALSE), collapse = "\n")
  ch <- strsplit(text, "", fixed = TRUE)[[1]]
  starts <- gregexpr('\\(defcard "', text)[[1]]
  if (starts[1] == -1) return(list())
  out <- vector("list", length(starts))
  k <- 0L
  for (s in starts) {
    e <- read_form(ch, s)
    if (is.na(e)) next
    body <- paste(ch[s:e], collapse = "")
    k <- k + 1L
    out[[k]] <- list(
      title = sub('^\\(defcard "([^"]*)".*$', "\\1", sub("\n.*$", "", body)),
      body = body
    )
  }
  out[seq_len(k)]
}

#' Count the top-level elements of a vector literal
#'
#' Counts forms, not characters: `[a (b c) "d"]` is three. Nested
#' delimiters and strings are skipped over wholesale.
#'
#' @param ch Character vector for the text containing the vector.
#' @param vstart Integer index of the vector's opening `[`.
#' @return Integer count, or `NA_integer_` if the vector never closes.
#' @keywords internal
count_form_elements <- function(ch, vstart) {
  vend <- read_form(ch, vstart)
  if (is.na(vend)) return(NA_integer_)
  if (vend <= vstart + 1L) return(0L)
  inner <- ch[(vstart + 1L):(vend - 1L)]

  n <- length(inner); i <- 1L; depth <- 0L; in_string <- FALSE
  count <- 0L; in_atom <- FALSE
  while (i <= n) {
    c1 <- inner[i]
    if (in_string) {
      if (c1 == "\\") i <- i + 1L else if (c1 == '"') in_string <- FALSE
    } else if (c1 == '"') {
      in_string <- TRUE
      if (depth == 0L && !in_atom) { count <- count + 1L; in_atom <- TRUE }
    } else if (c1 == "(" || c1 == "[" || c1 == "{") {
      if (depth == 0L) { count <- count + 1L; in_atom <- FALSE }
      depth <- depth + 1L
    } else if (c1 == ")" || c1 == "]" || c1 == "}") {
      depth <- depth - 1L
    } else if (depth == 0L) {
      if (c1 %in% CLOJURE_WHITESPACE) in_atom <- FALSE
      else if (!in_atom) { count <- count + 1L; in_atom <- TRUE }
    }
    i <- i + 1L
  }
  count
}

#' Count the elements of the first vector literal appearing after `key`
#'
#' @param body Character. One defcard form.
#' @param key Character. Literal text to search for, e.g. `":subroutines"`.
#' @return Integer count, or `NA_integer_` if `key` is absent or is not
#'   followed by a vector.
#' @keywords internal
count_vector_after <- function(body, key) {
  at <- regexpr(key, body, fixed = TRUE)
  if (at == -1) return(NA_integer_)
  ch <- strsplit(body, "", fixed = TRUE)[[1]]
  for (i in seq(at, length(ch))) {
    if (ch[i] == "[") return(count_form_elements(ch, i))
    # A non-vector value for the key -- e.g. `:subroutines subroutines`
    # inside a helper definition -- means there is nothing here to count.
    if (!(ch[i] %in% CLOJURE_WHITESPACE) && i > at + nchar(key)) return(NA_integer_)
  }
  NA_integer_
}
