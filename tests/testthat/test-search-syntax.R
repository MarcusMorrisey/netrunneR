# Covers the NetrunnerDB-compatible search grammar (R/search-syntax.R,
# R/search-fields.R, R/search-eval.R). Every documented worked example
# from https://netrunnerdb.com/en/syntax_new that our registry has the
# columns to express is exercised below, plus the operator/type matrix
# and the parse-error surface.

titles <- function(query) sort(search_filter(search_pool(), query)$title)

# ---- bare words and quoting -------------------------------------------

test_that("a bare word searches the title, case-insensitively", {
  expect_equal(titles("wall"), "Ice Wall")
  expect_equal(titles("ICE WALL"), "Ice Wall")
})

test_that("an empty or whitespace-only query filters nothing", {
  expect_equal(nrow(search_filter(search_pool(), "")), nrow(search_pool()))
  expect_equal(nrow(search_filter(search_pool(), "   ")), nrow(search_pool()))
})

test_that("a quoted phrase matches across its internal space", {
  expect_equal(titles('"wake up"'), "Wake Up Call")
  expect_equal(titles('x:"end the run"'), c("Enigma", "Ice Wall"))
})

test_that("string matching is literal, so regex metacharacters in a title are searchable as typed", {
  # "R&D Interface" would break or mis-match if plain values were
  # compiled as patterns. A bare word is never split on `&`, so this
  # needs no quoting; inside a field's value list it would be, hence the
  # quoted form below.
  expect_equal(titles("R&D"), "R&D Interface")
  expect_equal(titles('_:"R&D"'), "R&D Interface")
  # `(` is a grouping paren unless quoted -- same as NetrunnerDB.
  expect_equal(titles('x:"(your"'), "Wake Up Call")
})

# ---- fields, aliases, operators ---------------------------------------

test_that("canonical field names and their single-letter aliases agree", {
  expect_equal(titles("card_type:ice"), titles("t:ice"))
  expect_equal(titles("faction:nbn"), titles("f:nbn"))
  expect_equal(titles("card_subtype:barrier"), titles("s:barrier"))
})

test_that("integer fields support the full comparison set", {
  expect_equal(titles("o>4"), "Femme Fatale")
  expect_equal(titles("o>=9"), "Femme Fatale")
  expect_equal(titles("o:4"), "Tollbooth")
})

test_that("cost:X finds an X-cost card, which is stored as -1", {
  expect_equal(titles("o:X"), "Psychic Field")
  expect_equal(titles("o:x"), "Psychic Field")
})

test_that("an X cost participates in ordered comparisons as -1, per NRDB's stated encoding", {
  # NetrunnerDB documents X as "treated as -1 behind the scenes", so an
  # X-cost card genuinely sorts below every real cost and IS returned by
  # a `<` query. This is faithful behavior, not an off-by-one -- do not
  # "fix" it by special-casing -1 out of comparisons.
  expect_equal(titles("o<2"), c("Ice Wall", "Psychic Field"))
  expect_equal(titles("o<=1"), c("Ice Wall", "Psychic Field"))
  expect_false("Psychic Field" %in% titles("o>0"))
})

test_that("array fields match whole elements, not substrings of them", {
  expect_equal(titles("s:barrier"), "Ice Wall")
  # "Code Gate" is one element; "Gate" alone is not an element.
  expect_equal(titles('s:"code gate"'), c("Enigma", "Tollbooth"))
  expect_equal(titles("s:gate"), character(0))
})

test_that("the negated-match operator excludes", {
  expect_equal(titles("t:ice f!nbn"), "Ice Wall")
})

# ---- value lists ------------------------------------------------------

test_that("| inside a value list is OR", {
  expect_equal(titles("t:asset|hardware"), c("Psychic Field", "R&D Interface"))
})

test_that("& inside a value list is AND", {
  expect_equal(titles('x:"end the run"&run'), c("Enigma", "Ice Wall"))
  expect_equal(titles("x:psi&nonexistent"), character(0))
})

test_that("a negated value list excludes every listed value (NRDB's s!a|b|c example)", {
  expect_equal(titles('t:ice s!barrier|"code gate"'), character(0))
  expect_equal(titles("t:ice s!barrier"), c("Enigma", "Tollbooth"))
})

# ---- conjunctions, grouping, precedence -------------------------------

test_that("adjacent conditions are an implicit AND", {
  expect_equal(titles("t:ice f:nbn"), c("Enigma", "Tollbooth"))
})

test_that("explicit and/or behave like the implicit form", {
  expect_equal(titles("t:ice and f:nbn"), titles("t:ice f:nbn"))
  expect_equal(titles("f:shaper or f:criminal"), c("Femme Fatale", "R&D Interface"))
})

test_that("and binds tighter than or, per NRDB's documented precedence", {
  # f:anarch or (f:nbn and t:ice) -- NOT (f:anarch or f:nbn) and t:ice
  expect_equal(
    titles("f:anarch or f:nbn and t:ice"),
    c("Enigma", "Noise: Hacker Extraordinaire", "Tollbooth", "Wake Up Call")
  )
})

test_that("parentheses override precedence", {
  expect_equal(
    titles("(f:anarch or f:nbn) and t:ice"),
    c("Enigma", "Tollbooth")
  )
})

test_that("and/or keywords are case-insensitive", {
  expect_equal(titles("t:ice AND f:nbn"), titles("t:ice and f:nbn"))
  expect_equal(titles("f:shaper OR f:criminal"), titles("f:shaper or f:criminal"))
})

# ---- negation ---------------------------------------------------------

test_that("a ! or - prefix negates the whole condition", {
  expect_equal(titles("t:ice -f:nbn"), "Ice Wall")
  expect_equal(titles("t:ice !f:nbn"), "Ice Wall")
})

test_that("prefix negation and the ! operator cancel out when combined", {
  # -f!nbn is NOT(NOT nbn) == nbn
  expect_equal(titles("t:ice -f!nbn"), c("Enigma", "Tollbooth"))
})

# ---- regex ------------------------------------------------------------

test_that("/regex/ matches a string field as a pattern", {
  expect_equal(titles("_:/^Ice/"), "Ice Wall")
  expect_equal(titles("t:ice _:/^(En|To)/"), c("Enigma", "Tollbooth"))
})

test_that("a negated regex excludes matches", {
  expect_equal(titles("t:ice _!/^Ice/"), c("Enigma", "Tollbooth"))
})

test_that("regex is rejected on a non-string field", {
  expect_error(search_filter(search_pool(), "o:/[0-9]/"), class = "netrunneR_search_bad_operator")
})

# ---- titles that look like field queries ------------------------------

test_that("a colon inside an unquoted title is not mistaken for a field operator", {
  # "Noise:" has no registered field, and "Noise" is identifier-shaped,
  # so this is the case that must report a helpful error rather than
  # silently returning nothing.
  expect_error(search_filter(search_pool(), "Noise:Hacker"), class = "netrunneR_search_unknown_field")
  # Quoting is the documented way to search such a title.
  expect_equal(titles('"Noise: Hacker"'), "Noise: Hacker Extraordinaire")
})

# ---- error surface ----------------------------------------------------

test_that("an unknown field is reported by name, with the known fields listed", {
  err <- tryCatch(search_filter(search_pool(), "nonsense:x"), error = function(e) e)
  expect_s3_class(err, "netrunneR_search_unknown_field")
  expect_match(conditionMessage(err), "nonsense")
  expect_match(conditionMessage(err), "card_type")
})

test_that("an operator a field type does not accept is rejected", {
  expect_error(search_filter(search_pool(), "t<ice"), class = "netrunneR_search_bad_operator")
})

test_that("a non-numeric value for an integer field is rejected", {
  expect_error(search_filter(search_pool(), "o:cheap"), class = "netrunneR_search_bad_value")
})

test_that("unbalanced parentheses are reported", {
  expect_error(search_filter(search_pool(), "(t:ice"), class = "netrunneR_search_parse_error")
  expect_error(search_filter(search_pool(), "t:ice)"), class = "netrunneR_search_parse_error")
})

test_that("a dangling conjunction is reported", {
  expect_error(search_filter(search_pool(), "t:ice and"), class = "netrunneR_search_parse_error")
  expect_error(search_filter(search_pool(), "or f:nbn"), class = "netrunneR_search_parse_error")
})

# ---- registry ---------------------------------------------------------

test_that("the registry rejects a duplicate alias", {
  expect_error(
    search_field_registry(
      title = new_search_field("title", "string", aliases = "t"),
      card_type = new_search_field("type_code", "string", aliases = "t")
    ),
    class = "netrunneR_search_bad_field"
  )
})

test_that("an array field must declare a split delimiter", {
  expect_error(new_search_field("keywords", "array"), class = "netrunneR_search_bad_field")
})

test_that("a custom registry reuses the grammar against a different table", {
  # The reuse claim, exercised: same engine, different app's columns.
  people <- data.frame(
    nom = c("ada", "grace"), yr = c(1815L, 1906L), stringsAsFactors = FALSE
  )
  reg <- search_field_registry(
    name = new_search_field("nom", "string", aliases = "n"),
    year = new_search_field("yr", "integer", aliases = "y"),
    default_field = "name"
  )
  expect_equal(search_filter(people, "n:ada", reg)$nom, "ada")
  expect_equal(search_filter(people, "y>1900", reg)$nom, "grace")
  expect_equal(search_filter(people, "grace", reg)$nom, "grace")
})

test_that("a field mapped to a column the data lacks is reported", {
  reg <- search_field_registry(
    title = new_search_field("no_such_column", "string"),
    default_field = "title"
  )
  expect_error(
    search_filter(search_pool(), "anything", reg),
    class = "netrunneR_search_missing_column"
  )
})

# ---- explain ----------------------------------------------------------

test_that("search_explain() renders a query back in words", {
  expect_equal(search_explain(search_parse("")), "everything")
  expect_match(search_explain(search_parse("t:ice")), "card_type is 'ice'")
  expect_match(search_explain(search_parse("o<4")), "cost is less than '4'")
  expect_match(search_explain(search_parse("t:ice f:nbn")), "AND")
  expect_match(search_explain(search_parse("-t:ice")), "^NOT ")
})
