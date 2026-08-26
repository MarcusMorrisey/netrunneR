# Field registries for the search-syntax engine (R/search-syntax.R).
#
# The engine is generic; a registry is what binds it to one app's table.
# Another app -- a decklist browser, a tournament view, a different game
# entirely -- reuses the whole grammar by supplying its own registry
# instead of this one.

#' Define one searchable field
#'
#' @param column Character. Column name in the data being searched.
#' @param type One of "string", "integer", "date", "boolean", "array".
#'   Determines which operators are accepted and how values are compared
#'   (see the NetrunnerDB syntax reference: string fields do
#'   case-insensitive substring matching and also accept `/regex/`;
#'   integer and date fields accept `< <= > >=`; boolean fields accept
#'   true/false/t/f/1/0; array fields match individual elements exactly).
#' @param aliases Character vector of alternative names, e.g. `"t"` for
#'   `card_type`.
#' @param split Character. For `type = "array"`, the delimiter that
#'   splits a stored string into elements (the cardpool schema stores
#'   subtypes as a single `" - "`-delimited `keywords` column rather than
#'   a real array).
#' @return A field spec list.
#' @export
new_search_field <- function(column, type, aliases = character(0), split = NULL) {
  type <- match.arg(type, c("string", "integer", "date", "boolean", "array"))
  if (identical(type, "array") && is.null(split)) {
    rlang::abort("An array field needs a `split` delimiter.", class = "netrunneR_search_bad_field")
  }
  list(column = column, type = type, aliases = aliases, split = split)
}

#' Assemble a field registry from named field specs
#'
#' @param ... Named [new_search_field()] specs; the names are the
#'   canonical operand names.
#' @param default_field Character. The operand a bare word searches
#'   (NetrunnerDB uses the title).
#' @return A named list of field specs, carrying every alias as its own
#'   entry so lookup is a single named-list access, plus a
#'   `default_field` attribute.
#' @export
search_field_registry <- function(..., default_field = "title") {
  specs <- list(...)
  if (!length(specs) || is.null(names(specs)) || any(!nzchar(names(specs)))) {
    rlang::abort("Every field spec must be named.", class = "netrunneR_search_bad_field")
  }

  registry <- list()
  for (nm in names(specs)) {
    spec <- specs[[nm]]
    spec$name <- nm
    registry[[nm]] <- spec
    for (alias in spec$aliases) {
      if (!is.null(registry[[alias]])) {
        rlang::abort(
          sprintf("Search alias '%s' is claimed by more than one field.", alias),
          class = "netrunneR_search_bad_field"
        )
      }
      registry[[alias]] <- spec
    }
  }

  if (is.null(registry[[default_field]])) {
    rlang::abort(
      sprintf("default_field '%s' is not a registered field.", default_field),
      class = "netrunneR_search_bad_field"
    )
  }
  attr(registry, "default_field") <- default_field
  registry
}

#' Resolve an operand name (canonical or alias) to its field spec
#' @return A field spec, or NULL when the name is not registered.
#' @keywords internal
resolve_search_field <- function(name, fields) {
  fields[[tolower(name)]] %||% fields[[name]]
}

#' The search registry for the cardpool `card` table
#'
#' Operand names and single-letter aliases follow NetrunnerDB's syntax
#' (https://netrunnerdb.com/en/syntax_new) so a query a player already
#' knows works here unchanged.
#'
#' SCOPE: this registry covers the ten columns `inst/sql/schema/cardpool.sql`
#' actually mirrors. NetrunnerDB's remaining operands -- influence_cost,
#' agenda_points, advancement_cost, trash_cost, memory_usage, base_link,
#' illustrator, release_date, and the format/rotation/ban-list family
#' (format, card_pool, snapshot, is_banned, is_restricted, restriction_id)
#' -- are absent because the underlying data is not mirrored, not because
#' the engine cannot express them. Each becomes a one-line addition here
#' once its column exists; the grammar already supports every field type
#' they need.
#'
#' @return A field registry for use with [search_parse()] / [search_match()].
#' @export
cardpool_search_fields <- function() {
  do.call(search_field_registry, c(cardpool_search_field_specs(), list(default_field = "title")))
}

#' The card-table field specs, before assembly into a registry
#'
#' Separated from [cardpool_search_fields()] so another registry can
#' extend these rather than restate them -- [browser_search_fields()]
#' merges them with [legality_search_fields()]. An assembled registry
#' cannot be re-fed to [search_field_registry()], since it already has
#' every alias expanded into its own entry and a `default_field`
#' attribute attached; the specs are what compose.
#' @return A named list of [new_search_field()] specs.
#' @export
cardpool_search_field_specs <- function() {
  list(
    title        = new_search_field("title", "string", aliases = c("_")),
    text         = new_search_field("text", "string", aliases = c("x")),
    card_type    = new_search_field("type_code", "string", aliases = c("t", "type")),
    faction      = new_search_field("faction_code", "string", aliases = c("f")),
    side         = new_search_field("side_code", "string", aliases = c("d")),
    card_subtype = new_search_field("keywords", "array", aliases = c("s", "subtype"), split = " - "),
    card_set     = new_search_field("pack_code", "string", aliases = c("e", "set")),
    card_id      = new_search_field("code", "string", aliases = c("code")),
    cost         = new_search_field("cost", "integer", aliases = c("o")),
    strength     = new_search_field("strength", "integer", aliases = c("p"))
  )
}
