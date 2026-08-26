# Evaluates a search AST (R/search-syntax.R) against a data frame.
#
# Every matcher returns a logical vector the length of the column, with
# NA in the data treated as "does not match" rather than propagating NA
# -- a search result is a filter, so an unknown value is an exclusion,
# never a third state the caller has to handle.

#' Operators each field type accepts
#' @keywords internal
search_type_operators <- list(
  string  = c(":", "!"),
  array   = c(":", "!"),
  boolean = c(":", "!"),
  integer = c(":", "!", "<", "<=", ">", ">="),
  date    = c(":", "!", "<", "<=", ">", ">=")
)

#' @keywords internal
assert_operator_allowed <- function(op, spec) {
  allowed <- search_type_operators[[spec$type]]
  if (!op %in% allowed) {
    rlang::abort(
      sprintf(
        "Operator '%s' is not valid for %s field '%s' (accepts: %s).",
        op, spec$type, spec$name, paste(allowed, collapse = " ")
      ),
      class = "netrunneR_search_bad_operator"
    )
  }
  invisible(TRUE)
}

#' Compare two numeric/date vectors under a search operator
#' @keywords internal
compare_ordered <- function(col, value, op) {
  result <- switch(op,
    ":"  = col == value,
    "!"  = col == value,   # negation is applied by the caller
    "<"  = col < value,
    "<=" = col <= value,
    ">"  = col > value,
    ">=" = col >= value
  )
  result & !is.na(result)
}

#' Match one parsed value against a column, per the field's type
#' @keywords internal
match_search_value <- function(col, parsed, op, spec) {
  value <- parsed$value

  if (isTRUE(parsed$is_regex)) {
    if (!spec$type %in% c("string", "array")) {
      rlang::abort(
        sprintf("Regex matching is not supported for %s field '%s'.", spec$type, spec$name),
        class = "netrunneR_search_bad_operator"
      )
    }
    hits <- grepl(value, col, perl = TRUE)
    return(hits & !is.na(col))
  }

  switch(spec$type,
    string = {
      # Case-insensitive LITERAL substring, matching NetrunnerDB's
      # "lowercase both sides and wrap in %...%" behavior. fixed = TRUE
      # (rather than escaping the value into a pattern) means a title
      # containing regex metacharacters -- "Wake Up Call", "R&D
      # Interface", anything parenthesised -- searches as typed; only
      # /slashes/ opt into pattern syntax.
      hits <- grepl(tolower(value), tolower(col), fixed = TRUE)
      hits & !is.na(col)
    },
    array = {
      elements <- strsplit(ifelse(is.na(col), "", col), spec$split, fixed = TRUE)
      vapply(
        elements,
        function(el) any(tolower(trimws(el)) == tolower(value)),
        logical(1)
      )
    },
    boolean = {
      target <- parse_search_boolean(value, spec)
      hits <- as.logical(col) == target
      hits & !is.na(hits)
    },
    integer = {
      # NetrunnerDB stores an X cost as -1 and lets `cost:X` find it.
      target <- if (identical(tolower(value), "x")) -1L else suppressWarnings(as.integer(value))
      if (is.na(target)) {
        rlang::abort(
          sprintf("'%s' is not a whole number (field '%s').", value, spec$name),
          class = "netrunneR_search_bad_value"
        )
      }
      compare_ordered(suppressWarnings(as.integer(col)), target, op)
    },
    date = {
      target <- suppressWarnings(as.Date(value, format = "%Y-%m-%d"))
      if (is.na(target)) {
        rlang::abort(
          sprintf("'%s' is not a YYYY-MM-DD date (field '%s').", value, spec$name),
          class = "netrunneR_search_bad_value"
        )
      }
      compare_ordered(suppressWarnings(as.Date(col)), target, op)
    }
  )
}

#' @keywords internal
parse_search_boolean <- function(value, spec) {
  v <- tolower(value)
  if (v %in% c("true", "t", "1")) return(TRUE)
  if (v %in% c("false", "f", "0")) return(FALSE)
  rlang::abort(
    sprintf("'%s' is not a boolean (field '%s'); use true/false/t/f/1/0.", value, spec$name),
    class = "netrunneR_search_bad_value"
  )
}

#' Evaluate one condition node
#' @keywords internal
eval_search_condition <- function(node, data) {
  spec <- node$field
  assert_operator_allowed(node$op, spec)

  if (is.null(data[[spec$column]])) {
    rlang::abort(
      sprintf("Search field '%s' maps to missing column '%s'.", spec$name, spec$column),
      class = "netrunneR_search_missing_column"
    )
  }
  col <- data[[spec$column]]

  per_value <- lapply(node$values, match_search_value, col = col, op = node$op, spec = spec)
  combine <- if (identical(node$combinator, "&")) `&` else `|`
  hits <- Reduce(combine, per_value)

  # `field!value` and a `-`/`!` prefix are independent negations, so a
  # doubly-negated term (`-f!anarch`) correctly reads as a plain match.
  if (xor(identical(node$op, "!"), isTRUE(node$negate))) !hits else hits
}

#' Evaluate a parsed search AST against a data frame
#'
#' @param ast The value returned by [search_parse()]; NULL matches every
#'   row (an empty query is not a filter).
#' @param data A data frame whose columns the registry's fields name.
#' @return A logical vector with one element per row of `data`.
#' @export
search_match <- function(ast, data) {
  if (is.null(ast)) return(rep(TRUE, nrow(data)))

  switch(ast$type,
    and  = search_match(ast$children[[1]], data) & search_match(ast$children[[2]], data),
    or   = search_match(ast$children[[1]], data) | search_match(ast$children[[2]], data),
    cond = eval_search_condition(ast, data),
    rlang::abort(sprintf("Unknown search AST node '%s'.", ast$type), class = "netrunneR_search_parse_error")
  )
}

#' Filter a data frame with a NetrunnerDB-style search query
#'
#' The one call an app normally needs: parse, evaluate, subset. Parse and
#' value errors surface as conditions classed `netrunneR_search_*` so a
#' UI can show the message rather than a stack trace -- see
#' [search_explain()] for turning a query into a human-readable summary.
#'
#' @param data A data frame to filter.
#' @param query Character scalar in NetrunnerDB search syntax. An empty
#'   or whitespace-only query returns `data` unchanged.
#' @param fields A field registry; defaults to [cardpool_search_fields()].
#' @return `data`, filtered to matching rows.
#' @export
search_filter <- function(data, query, fields = cardpool_search_fields()) {
  if (!nzchar(trimws(query %||% ""))) return(data)
  ast <- search_parse(query, fields)
  data[search_match(ast, data), , drop = FALSE]
}

#' Describe a parsed query in words
#'
#' Lets a search UI echo back what it understood ("type is ice AND cost
#' less than 4"), which is the difference between a typo returning zero
#' results mysteriously and the user seeing why.
#'
#' @param ast The value returned by [search_parse()].
#' @return A character scalar.
#' @export
search_explain <- function(ast) {
  if (is.null(ast)) return("everything")

  switch(ast$type,
    and = sprintf("(%s AND %s)", search_explain(ast$children[[1]]), search_explain(ast$children[[2]])),
    or  = sprintf("(%s OR %s)", search_explain(ast$children[[1]]), search_explain(ast$children[[2]])),
    cond = {
      verb <- switch(ast$op,
        ":"  = "is",
        "!"  = "is not",
        "<"  = "is less than",
        "<=" = "is at most",
        ">"  = "is greater than",
        ">=" = "is at least"
      )
      joiner <- if (identical(ast$combinator, "&")) " and " else " or "
      shown <- vapply(ast$values, function(v) {
        if (isTRUE(v$is_regex)) sprintf("matching /%s/", v$value) else sprintf("'%s'", v$value)
      }, character(1))
      phrase <- sprintf("%s %s %s", ast$field$name, verb, paste(shown, collapse = joiner))
      if (isTRUE(ast$negate)) sprintf("NOT (%s)", phrase) else phrase
    },
    "everything"
  )
}
