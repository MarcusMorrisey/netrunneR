# Covers rotation / ban-list legality (R/legality.R) and the readers that
# flatten upstream's nested rotations.json / mwl.json into the long child
# tables the schema declares (read_rotations() / read_mwl() in
# R/build-cardpool.R).

legality_fixture <- function() {
  list(
    rotation = tibble::tribble(
      ~code,          ~name,             ~date_start,
      "rotation-old", "Older Rotation",  "2020-01-01",
      "rotation-now", "Current Rotation","2025-04-24",
      "rotation-next","Future Rotation", "2099-01-01"
    ),
    rotation_cycle = tibble::tribble(
      ~rotation_code, ~cycle_code,
      "rotation-old", "ancient",
      "rotation-now", "ancient",
      "rotation-now", "old_cycle"
    ),
    mwl = tibble::tribble(
      ~code,           ~name,               ~format,    ~date_start,
      "standard-old",  "Standard Old",      "standard", "2024-01-01",
      "standard-now",  "Standard Current",  "standard", "2026-08-01",
      "standard-next", "Standard Future",   "standard", "2099-01-01",
      "startup-now",   "Startup Current",   "startup",  "2026-05-01"
    ),
    mwl_card = tibble::tribble(
      ~mwl_code,      ~card_code, ~deck_limit, ~is_restricted, ~universal_faction_cost, ~global_penalty,
      "standard-now", "c_ban",    0L,          NA_integer_,    NA_integer_,             NA_integer_,
      "standard-now", "c_res",    NA_integer_, 1L,             NA_integer_,             NA_integer_,
      "standard-now", "c_inf",    NA_integer_, NA_integer_,    3L,                      NA_integer_,
      "standard-old", "c_clean",  0L,          NA_integer_,    NA_integer_,             NA_integer_
    ),
    packs = tibble::tribble(
      ~code,     ~cycle_code,
      "pk_live", "live_cycle",
      "pk_old",  "old_cycle"
    ),
    cards = tibble::tribble(
      ~code,     ~title,       ~pack_code,
      "c_ban",   "Banned",     "pk_live",
      "c_res",   "Restricted", "pk_live",
      "c_inf",   "Influenced", "pk_live",
      "c_clean", "Clean",      "pk_live",
      "c_rot",   "Rotated",    "pk_old"
    )
  )
}

# ---- active_rotation / active_mwl --------------------------------------

test_that("active_rotation() picks the most recent rotation already in force", {
  f <- legality_fixture()
  expect_equal(active_rotation(f$rotation, as.Date("2026-08-26"))$code, "rotation-now")
})

test_that("active_rotation() ignores a rotation whose start date has not arrived", {
  f <- legality_fixture()
  expect_equal(active_rotation(f$rotation, as.Date("2021-01-01"))$code, "rotation-old")
})

test_that("active_rotation() returns NULL when nothing is in force yet", {
  f <- legality_fixture()
  expect_null(active_rotation(f$rotation, as.Date("1999-01-01")))
  expect_null(active_rotation(f$rotation[0, ], as.Date("2026-08-26")))
})

test_that("active_mwl() defaults to Standard and takes the newest list in force", {
  f <- legality_fixture()
  expect_equal(active_mwl(f$mwl, as_of = as.Date("2026-08-26"))$code, "standard-now")
  expect_equal(active_mwl(f$mwl, as_of = as.Date("2025-01-01"))$code, "standard-old")
})

test_that("active_mwl() selects per format, not across formats", {
  f <- legality_fixture()
  expect_equal(active_mwl(f$mwl, "startup", as.Date("2026-08-26"))$code, "startup-now")
  expect_null(active_mwl(f$mwl, "napd", as.Date("2026-08-26")))
})

# ---- annotate_legality -------------------------------------------------

test_that("annotate_legality() flags banned, restricted, influenced and rotated cards", {
  f <- legality_fixture()
  ann <- annotate_legality(
    f$cards, f$packs, f$rotation_cycle, f$mwl_card,
    active_rotation(f$rotation, as.Date("2026-08-26")),
    active_mwl(f$mwl, as_of = as.Date("2026-08-26"))
  )

  expect_equal(ann$is_banned,         c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(ann$is_restricted,     c(FALSE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(ann$influence_penalty, c(0L, 0L, 3L, 0L, 0L))
  expect_equal(ann$in_rotation,       c(TRUE, TRUE, TRUE, TRUE, FALSE))
})

test_that("annotate_legality() reads only the supplied list, not every list in the table", {
  # c_clean is banned on standard-old but NOT on the active standard-now.
  f <- legality_fixture()
  ann <- annotate_legality(
    f$cards, f$packs, f$rotation_cycle, f$mwl_card,
    active_rotation(f$rotation, as.Date("2026-08-26")),
    active_mwl(f$mwl, as_of = as.Date("2026-08-26"))
  )
  expect_false(ann$is_banned[ann$code == "c_clean"])
})

test_that("annotate_legality() annotates rather than filters, so a banned card stays visible", {
  f <- legality_fixture()
  ann <- annotate_legality(
    f$cards, f$packs, f$rotation_cycle, f$mwl_card,
    active_rotation(f$rotation, as.Date("2026-08-26")),
    active_mwl(f$mwl, as_of = as.Date("2026-08-26"))
  )
  expect_equal(nrow(ann), nrow(f$cards))
})

test_that("a NULL rotation or ban list excludes nothing", {
  f <- legality_fixture()
  ann <- annotate_legality(f$cards, f$packs, f$rotation_cycle, f$mwl_card, NULL, NULL)
  expect_true(all(ann$in_rotation))
  expect_false(any(ann$is_banned))
  expect_true(all(ann$influence_penalty == 0L))
})

# ---- filter_legal ------------------------------------------------------

test_that("filter_legal() drops banned and rotated cards but keeps restricted and influenced ones", {
  # Restricted/influenced are deckbuilding constraints, not exclusions.
  f <- legality_fixture()
  ann <- annotate_legality(
    f$cards, f$packs, f$rotation_cycle, f$mwl_card,
    active_rotation(f$rotation, as.Date("2026-08-26")),
    active_mwl(f$mwl, as_of = as.Date("2026-08-26"))
  )
  expect_equal(sort(filter_legal(ann)$title), c("Clean", "Influenced", "Restricted"))
})

# ---- upstream readers --------------------------------------------------

test_that("read_rotations() flattens the rotated array into one row per cycle", {
  path <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(
    list(code = "r1", name = "One", date_start = "2020-01-01", rotated = c("a", "b")),
    list(code = "r2", name = "Two", date_start = "2025-01-01", rotated = character(0))
  ), auto_unbox = TRUE), path)

  out <- read_rotations(path)
  expect_equal(nrow(out$rotation), 2)
  expect_equal(nrow(out$rotation_cycle), 2)
  expect_equal(out$rotation_cycle$cycle_code, c("a", "b"))
})

test_that("read_mwl() walks the card-keyed object into one row per card", {
  # The `cards` OBJECT is keyed by card code -- jsonlite's data-frame
  # simplification would turn it into one COLUMN per card, which is the
  # shape that broke the removed decklist feature.
  path <- withr::local_tempfile(fileext = ".json")
  writeLines(jsonlite::toJSON(list(
    list(code = "standard-x", name = "Std X", date_start = "2026-01-01",
         cards = list(`01001` = list(deck_limit = 0), `01002` = list(is_restricted = 1)))
  ), auto_unbox = TRUE), path)

  out <- read_mwl(path)
  expect_equal(out$mwl$format, "standard")
  expect_equal(nrow(out$mwl_card), 2)
  expect_equal(out$mwl_card$card_code, c("01001", "01002"))
  expect_equal(out$mwl_card$deck_limit, c(0L, NA_integer_))
  expect_equal(out$mwl_card$is_restricted, c(NA_integer_, 1L))
})

test_that("mwl_format_of() derives every prefix upstream actually uses, and flags anything else", {
  expect_equal(mwl_format_of("standard-ban-list-26-05"), "standard")
  expect_equal(mwl_format_of("startup-balance-update-26-03-for-classic-only"), "startup")
  expect_equal(mwl_format_of("sunset-ban-list-24-01"), "sunset")
  expect_equal(mwl_format_of("NAPD_MWL_2.1"), "napd")
  expect_equal(mwl_format_of("brandnew-list-99"), "unknown")
})

# ---- legality fields through the search grammar ------------------------

test_that("legality columns are searchable in the same query language", {
  f <- legality_fixture()
  ann <- annotate_legality(
    f$cards, f$packs, f$rotation_cycle, f$mwl_card,
    active_rotation(f$rotation, as.Date("2026-08-26")),
    active_mwl(f$mwl, as_of = as.Date("2026-08-26"))
  )
  reg <- do.call(search_field_registry, c(
    list(title = new_search_field("title", "string", aliases = "_")),
    legality_search_fields(),
    list(default_field = "title")
  ))

  expect_equal(search_filter(ann, "is_banned:true", reg)$title, "Banned")
  expect_equal(search_filter(ann, "banned:false in_rotation:true", reg)$title,
               c("Restricted", "Influenced", "Clean"))
  expect_equal(search_filter(ann, "influence_penalty>0", reg)$title, "Influenced")
})
