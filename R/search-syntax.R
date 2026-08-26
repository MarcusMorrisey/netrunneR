# NetrunnerDB-compatible search syntax: tokenizer, parser and evaluator.
#
# Deliberately split into a GENERIC grammar engine (this file's tokenize/
# parse/evaluate functions, which know nothing about Netrunner) and an
# app-supplied FIELD REGISTRY (search-fields.R) mapping operand names to
# columns and types. That seam is the whole point: the grammar below is
# NetrunnerDB's, but any app with a different table can reuse it by
# passing its own registry -- see new_search_field().
#
# Grammar (mirrors https://netrunnerdb.com/en/syntax_new):
#
#   expr      := or_expr
#   or_expr   := and_expr ( "or" and_expr )*
#   and_expr  := unary ( ( "and" | <implicit space> ) unary )*
#   unary     := ( "!" | "-" ) unary | "(" expr ")" | condition
#   condition := field operator valuelist | bareword
#   valuelist := value ( ( "|" | "&" ) value )*
#   value     := '"' ... '"' | "/" regex "/" | word
#
# "and" (explicit or implicit) binds tighter than "or", per NRDB.

#' Tokenize a search query
#'
#' Splits on whitespace and parentheses while treating `"quoted strings"`
#' and `/regex/` as atomic (so a quoted phrase may contain spaces, and a
#' regex may contain parens and operator characters without being split).
#' Emits `term`, `lparen`, `rparen`, `and` and `or` tokens.
#'
#' @param query Character scalar.
#' @return A list of token lists, each with `type` and (for terms) `text`.
#' @keywords internal
search_tokenize <- function(query) {
  chars <- strsplit(query, "", fixed = TRUE)[[1]]
  n <- length(chars)
  tokens <- list()
  buf <- character(0)

  flush_buf <- function() {
    if (!length(buf)) return(invisible(NULL))
    text <- paste0(buf, collapse = "")
    buf <<- character(0)
    kind <- switch(tolower(text), and = "and", or = "or", "term")
    tokens[[length(tokens) + 1L]] <<- if (identical(kind, "term")) {
      list(type = "term", text = text)
    } else {
      list(type = kind)
    }
    invisible(NULL)
  }

  i <- 1L
  while (i <= n) {
    ch <- chars[i]

    # Quoted string and regex literals are consumed whole, delimiters
    # included -- parse_term() strips them once it knows the context.
    if (ch == '"' || ch == "/") {
      closer <- ch
      j <- i + 1L
      while (j <= n && chars[j] != closer) j <- j + 1L
      last <- min(j, n)
      buf <- c(buf, chars[i:last])
      i <- last + 1L
      next
    }

    if (grepl("^\\s$", ch)) {
      flush_buf()
      i <- i + 1L
      next
    }

    if (ch == "(" || ch == ")") {
      flush_buf()
      tokens[[length(tokens) + 1L]] <- list(type = if (ch == "(") "lparen" else "rparen")
      i <- i + 1L
      next
    }

    buf <- c(buf, ch)
    i <- i + 1L
  }
  flush_buf()

  tokens
}

#' Find a query term's field/value operator, ignoring quoted and regex spans
#'
#' Two-character operators are checked before their one-character
#' prefixes so `n<=3` reads as `<=`, not `<` followed by a stray `=`.
#' @return A list with `op` and `at` (1-based index), or NULL if the term
#'   carries no operator (a bare word).
#' @keywords internal
find_term_operator <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (ch == '"' || ch == "/") {
      closer <- ch
      j <- i + 1L
      while (j <= n && chars[j] != closer) j <- j + 1L
      i <- min(j, n) + 1L
      next
    }
    two <- if (i < n) paste0(chars[i], chars[i + 1L]) else ""
    if (two %in% c("<=", ">=")) return(list(op = two, at = i))
    if (ch %in% c(":", "!", "<", ">")) return(list(op = ch, at = i))
    i <- i + 1L
  }
  NULL
}

#' Split a value list on `|` / `&`, ignoring quoted and regex spans
#' @return A list with `values` (character vector) and `combinator`.
#' @keywords internal
split_value_list <- function(text) {
  chars <- strsplit(text, "", fixed = TRUE)[[1]]
  n <- length(chars)
  parts <- character(0)
  combinators <- character(0)
  buf <- character(0)
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (ch == '"' || ch == "/") {
      closer <- ch
      j <- i + 1L
      while (j <= n && chars[j] != closer) j <- j + 1L
      last <- min(j, n)
      buf <- c(buf, chars[i:last])
      i <- last + 1L
      next
    }
    if (ch == "|" || ch == "&") {
      parts <- c(parts, paste0(buf, collapse = ""))
      combinators <- c(combinators, ch)
      buf <- character(0)
      i <- i + 1L
      next
    }
    buf <- c(buf, ch)
    i <- i + 1L
  }
  parts <- c(parts, paste0(buf, collapse = ""))

  # NRDB documents | and & as alternatives, not as a mixed-precedence
  # pair; a term using both is ambiguous, so take the first seen rather
  # than silently inventing precedence.
  combinator <- if (length(combinators)) combinators[[1]] else "|"
  list(values = parts, combinator = combinator)
}

#' Strip quoting from one value, flagging regex literals
#' @return A list with `value` and `is_regex`.
#' @keywords internal
parse_value <- function(text) {
  if (nchar(text) >= 2L && startsWith(text, "/") && endsWith(text, "/")) {
    return(list(value = substr(text, 2L, nchar(text) - 1L), is_regex = TRUE))
  }
  if (nchar(text) >= 2L && startsWith(text, '"') && endsWith(text, '"')) {
    return(list(value = substr(text, 2L, nchar(text) - 1L), is_regex = FALSE))
  }
  # An unterminated quote (the tokenizer ran to end-of-input) still gets
  # its opening delimiter removed, so `x:"end the run` behaves like the
  # closed form rather than searching for a literal quote character.
  if (startsWith(text, '"')) return(list(value = substring(text, 2L), is_regex = FALSE))
  if (startsWith(text, "/")) return(list(value = substring(text, 2L), is_regex = TRUE))
  list(value = text, is_regex = FALSE)
}

#' Parse one term token into a condition node
#' @keywords internal
parse_term <- function(text, fields) {
  negate <- FALSE
  while (nchar(text) > 1L && (startsWith(text, "!") || startsWith(text, "-"))) {
    negate <- !negate
    text <- substring(text, 2L)
  }

  op_info <- find_term_operator(text)
  bare <- function() {
    parsed <- parse_value(text)
    list(
      # The resolved spec, not the field's name: every downstream
      # consumer (the evaluator, search_explain()) expects a spec.
      type = "cond",
      field = resolve_search_field(attr(fields, "default_field"), fields),
      op = ":", values = list(parsed), combinator = "|", negate = negate
    )
  }

  if (is.null(op_info) || op_info$at == 1L) return(bare())

  field_name <- substr(text, 1L, op_info$at - 1L)
  rest <- substring(text, op_info$at + nchar(op_info$op))
  resolved <- resolve_search_field(field_name, fields)

  if (is.null(resolved)) {
    # A bare word that merely happens to contain a colon -- a card title
    # like "Noise: Hacker Extraordinaire" -- must not be mistaken for a
    # field query. Only something shaped like an identifier is treated as
    # a misspelled field and reported; anything else falls back to a
    # plain title search.
    if (grepl("^[A-Za-z_][A-Za-z0-9_]*$", field_name)) {
      rlang::abort(
        sprintf(
          "Unknown search field '%s'. Known fields: %s",
          field_name, paste(sort(names(fields)), collapse = ", ")
        ),
        class = "netrunneR_search_unknown_field"
      )
    }
    return(bare())
  }

  split <- split_value_list(rest)
  list(
    type = "cond",
    field = resolved,
    op = op_info$op,
    values = lapply(split$values, parse_value),
    combinator = split$combinator,
    negate = negate
  )
}

#' Parse a tokenized query into an abstract syntax tree
#' @keywords internal
search_parse_tokens <- function(tokens, fields) {
  pos <- 1L

  peek <- function() if (pos <= length(tokens)) tokens[[pos]] else NULL
  take <- function() {
    tok <- tokens[[pos]]
    pos <<- pos + 1L
    tok
  }

  parse_expr <- function() {
    node <- parse_and()
    while (!is.null(peek()) && identical(peek()$type, "or")) {
      take()
      rhs <- parse_and()
      node <- list(type = "or", children = list(node, rhs))
    }
    node
  }

  parse_and <- function() {
    node <- parse_unary()
    repeat {
      tok <- peek()
      if (is.null(tok)) break
      if (identical(tok$type, "and")) {
        take()
      } else if (!tok$type %in% c("term", "lparen")) {
        # "or" or a closing paren ends this and-chain.
        break
      }
      # A bare term or "(" following another term is an implicit AND.
      rhs <- parse_unary()
      node <- list(type = "and", children = list(node, rhs))
    }
    node
  }

  parse_unary <- function() {
    tok <- peek()
    if (is.null(tok)) {
      rlang::abort("Unexpected end of search query.", class = "netrunneR_search_parse_error")
    }
    if (identical(tok$type, "lparen")) {
      take()
      node <- parse_expr()
      closing <- peek()
      if (is.null(closing) || !identical(closing$type, "rparen")) {
        rlang::abort("Unbalanced '(' in search query.", class = "netrunneR_search_parse_error")
      }
      take()
      return(node)
    }
    if (identical(tok$type, "rparen")) {
      rlang::abort("Unbalanced ')' in search query.", class = "netrunneR_search_parse_error")
    }
    if (tok$type %in% c("and", "or")) {
      rlang::abort(
        sprintf("Search query has a dangling '%s'.", tok$type),
        class = "netrunneR_search_parse_error"
      )
    }
    parse_term(take()$text, fields)
  }

  node <- parse_expr()
  if (pos <= length(tokens)) {
    rlang::abort("Unbalanced ')' in search query.", class = "netrunneR_search_parse_error")
  }
  node
}

#' Parse a search query string into an abstract syntax tree
#'
#' @param query Character scalar in NetrunnerDB search syntax.
#' @param fields A field registry from [cardpool_search_fields()] or
#'   [new_search_field()].
#' @return An AST list, or NULL for an empty query (matches everything).
#' @export
search_parse <- function(query, fields = cardpool_search_fields()) {
  stopifnot(is.character(query), length(query) == 1L)
  tokens <- search_tokenize(query)
  if (!length(tokens)) return(NULL)
  search_parse_tokens(tokens, fields)
}
