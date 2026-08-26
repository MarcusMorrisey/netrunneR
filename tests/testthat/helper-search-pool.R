# Fixture for the search-syntax tests: a handful of cards shaped exactly
# like the real cardpool `card` table (inst/sql/schema/cardpool.sql), with
# enough variety to exercise every field type the registry declares --
# integer (cost/strength, including an X cost stored as -1), array
# (keywords), and string (title/text/faction/type/side), plus a title
# containing regex metacharacters and one containing a colon.

search_pool <- function() {
  tibble::tribble(
    ~code,   ~title,                ~pack_code, ~faction_code, ~type_code, ~side_code, ~text,                        ~cost, ~strength, ~keywords,
    "01001", "Ice Wall",            "core",     "weyland",     "ice",      "corp",     "End the run.",               1L,    1L,        "Barrier - Advanceable",
    "01002", "Tollbooth",           "core",     "nbn",         "ice",      "corp",     "The Runner must pay 3.",     4L,    5L,        "Code Gate",
    "01003", "Enigma",              "core",     "nbn",         "ice",      "corp",     "End the run.",               3L,    2L,        "Code Gate",
    "01004", "Wake Up Call",        "sg",       "anarch",      "event",    "runner",   "Trash a card (your call).",  2L,    NA_integer_, "Double",
    "01005", "R&D Interface",       "core",     "shaper",      "hardware", "runner",   "Access an extra card.",      3L,    NA_integer_, "Console",
    "01006", "Noise: Hacker Extraordinaire", "core", "anarch", "identity", "runner",   "Whenever you install.",      NA_integer_, NA_integer_, "G-mod",
    "01007", "Femme Fatale",        "core",     "criminal",    "program",  "runner",   "Bypass a piece of ice.",     9L,    2L,        "Icebreaker - Killer",
    "01008", "Psychic Field",       "core",     "jinteki",     "asset",    "corp",     "Psi ability.",               -1L,   NA_integer_, "Ambush - Psi"
  )
}
