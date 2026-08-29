# Tests build_implementation()'s trait extraction and release_id shape
# against a minimal fixture card-definition file, matching the real
# mtgred/netrunner layout: Clojure source grouped one file per card
# category under src/clj/game/cards/.
#
# The fixture forms below are copied in shape from the real ice.clj and
# programs.clj, including the two that must NOT parse -- a fixture that
# only contains readable cards cannot show that an unreadable one is
# recorded rather than guessed at.

fixture_ice_clj <- '
(ns game.cards.ice)

(defcard "Cheap Wall"
  {:subroutines [end-the-run]})

(defcard "Expensive Code"
  {:subroutines [(gain-credits-sub 1)
                 end-the-run]})

(defcard "Walled Off"
  (wall-ice [end-the-run end-the-run]))

(defcard "Grail Thing"
  (grail-ice some-ability))

(defcard "Untouched Gate"
  (variable-subs-ice
    (fn [state] (count (get-in @state [:corp :hand])))
    end-the-run))
'

fixture_programs_clj <- '
(ns game.cards.programs)

(defcard "Bargain Breaker"
  (auto-icebreaker {:abilities [(break-sub 1 1 "Barrier")
                                (strength-pump 1 1)]}))

(defcard "Fixed Breaker"
  {:abilities [(break-sub 1 1 "Barrier")]})

(defcard "Counter Breaker"
  {:abilities [(break-sub [(->c :virus 1)] 1 "Sentry")]})

(defcard "Not A Breaker"
  {:abilities [(some-other-thing 1)]})
'

write_fixture_tree <- function() {
  raw_dir <- withr::local_tempdir(.local_envir = parent.frame())
  cards_dir <- file.path(raw_dir, "src", "clj", "game", "cards")
  fs::dir_create(cards_dir)
  writeLines(fixture_ice_clj, file.path(cards_dir, "ice.clj"))
  writeLines(fixture_programs_clj, file.path(cards_dir, "programs.clj"))
  raw_dir
}

test_that("subroutines are counted per card, including through wall-ice and grail-ice", {
  cards_dir <- file.path(write_fixture_tree(), "src", "clj", "game", "cards")
  ice <- extract_traits_from_file(file.path(cards_dir, "ice.clj"), "ice")

  counts <- stats::setNames(ice$subroutine_count, ice$title)
  expect_equal(counts[["Cheap Wall"]], 1L)
  expect_equal(counts[["Expensive Code"]], 2L)
  # wall-ice passes its vector straight through to :subroutines.
  expect_equal(counts[["Walled Off"]], 2L)
  # grail-ice wraps a single ability, always exactly one.
  expect_equal(counts[["Grail Thing"]], 1L)
})

test_that("an ice whose subroutine count is variable is recorded as such, not guessed", {
  cards_dir <- file.path(write_fixture_tree(), "src", "clj", "game", "cards")
  ice <- extract_traits_from_file(file.path(cards_dir, "ice.clj"), "ice")
  row <- ice[ice$title == "Untouched Gate", ]

  expect_true(is.na(row$subroutine_count))
  # The status is the point: a NULL that says why is reportable, a bare
  # NULL is indistinguishable from a card nobody has looked at.
  expect_equal(row$parse_status, "variable_subroutines")
})

test_that("breaker economics are read from break-sub and strength-pump", {
  cards_dir <- file.path(write_fixture_tree(), "src", "clj", "game", "cards")
  progs <- extract_traits_from_file(file.path(cards_dir, "programs.clj"), "program")
  row <- progs[progs$title == "Bargain Breaker", ]

  expect_equal(row$break_cost, 1L)
  expect_equal(row$break_qty, 1L)
  expect_equal(row$break_subtype, "Barrier")
  expect_equal(row$pump_cost, 1L)
  expect_equal(row$pump_amount, 1L)
  expect_equal(row$parse_status, "parsed")
})

test_that("a breaker with no strength-pump is parsed, not treated as a failure", {
  # Fixed strength is a real card design (Atman, Begemot), so it gets its
  # own status rather than being lumped in with forms that defeated the
  # parser.
  cards_dir <- file.path(write_fixture_tree(), "src", "clj", "game", "cards")
  progs <- extract_traits_from_file(file.path(cards_dir, "programs.clj"), "program")
  row <- progs[progs$title == "Fixed Breaker", ]

  expect_equal(row$break_cost, 1L)
  expect_true(is.na(row$pump_cost))
  expect_equal(row$parse_status, "parsed_no_pump")
})

test_that("a non-credit break cost keeps its subtype but reports no cost", {
  # Charging credits for a card that spends virus counters would be a
  # wrong number; dropping the row would hide the pair entirely. Keeping
  # the subtype means the matchup still appears, as not_computable.
  cards_dir <- file.path(write_fixture_tree(), "src", "clj", "game", "cards")
  progs <- extract_traits_from_file(file.path(cards_dir, "programs.clj"), "program")
  row <- progs[progs$title == "Counter Breaker", ]

  expect_true(is.na(row$break_cost))
  expect_equal(row$break_subtype, "Sentry")
  expect_equal(row$parse_status, "non_credit_break_cost")
})

test_that("a program with no break clause is not an icebreaker and gets no row", {
  cards_dir <- file.path(write_fixture_tree(), "src", "clj", "game", "cards")
  progs <- extract_traits_from_file(file.path(cards_dir, "programs.clj"), "program")
  expect_false("Not A Breaker" %in% progs$title)
})

test_that("titles are expanded to every cardpool code carrying them", {
  # Behaviour belongs to the card, not the printing, so a reprint gets the
  # same traits under its own code.
  traits <- tibble::tibble(
    title = "Cheap Wall", kind = "ice", subroutine_count = 1L,
    break_cost = NA_integer_, break_qty = NA_integer_,
    break_subtype = NA_character_, pump_cost = NA_integer_,
    pump_amount = NA_integer_, parse_status = "parsed"
  )
  cardpool <- tibble::tribble(
    ~code,   ~title,
    "ice01", "Cheap Wall",
    "ice99", "Cheap Wall",
    "oth01", "Something Else"
  )

  keyed <- attach_cardpool_codes(traits, cardpool)
  expect_setequal(keyed$code, c("ice01", "ice99"))
  expect_true(all(keyed$subroutine_count == 1L))
})

test_that("build_implementation() writes the traits table and stamps the release", {
  raw_dir <- write_fixture_tree()
  li <- new_lineage("implementation", "git_mirror", withr::local_tempdir(), schema_version = 2L,
                    build_module_path = "R/build-implementation.R")
  staged_raw <- list(raw_dir = raw_dir, source_revision = "def456")

  built <- build_implementation(li, staged_raw)

  expect_true(fs::file_exists(built$db_path))
  expect_match(built$release_id, "^def456-b")

  con <- DBI::dbConnect(RSQLite::SQLite(), built$db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  cols <- DBI::dbListFields(con, "ice_breaker_traits")
  expect_true(all(c("code", "title", "kind", "subroutine_count", "break_cost",
                    "break_qty", "break_subtype", "pump_cost", "pump_amount",
                    "parse_status") %in% cols))
})
