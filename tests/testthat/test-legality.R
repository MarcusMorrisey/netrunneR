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
  # "eternal" is a real format; the fixture just has no list for it.
  # (It used to say "napd", which is not a format at all -- see the v2
  # tests below.)
  expect_null(active_mwl(f$mwl, "eternal", as.Date("2026-08-26")))
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
  # No format column: read_mwl() no longer guesses one, and
  # build_cardpool() attaches it by joining to the v2 restriction table.
  expect_false("format" %in% names(out$mwl))
  expect_equal(nrow(out$mwl_card), 2)
  expect_equal(out$mwl_card$card_code, c("01001", "01002"))
  expect_equal(out$mwl_card$deck_limit, c(0L, NA_integer_))
  expect_equal(out$mwl_card$is_restricted, c(NA_integer_, 1L))
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

# ---- v2 format / card-pool / restriction model -------------------------
#
# Covers read_formats() / read_card_pools() / read_restrictions() /
# read_card_sets() / read_printings() / mwl_v2_format() in
# R/build-cardpool.R, and active_snapshot() / annotate_format_legality()
# in R/legality.R.

format_fixture <- function() {
  list(
    format_snapshot = tibble::tribble(
      ~id,           ~format_id,  ~date_start,  ~card_pool_id, ~restriction_id,  ~is_active,
      "std_old",     "standard",  "2024-01-01", "pool_old",    "ban_old",        0L,
      "std_now",     "standard",  "2026-08-01", "pool_now",    "ban_now",        0L,
      "std_next",    "standard",  "2099-01-01", "pool_next",   "ban_next",       1L,
      "start_now",   "startup",   "2026-05-01", "pool_start",  "ban_start",      1L,
      "core_now",    "core",      "2025-04-24", "pool_core",   NA_character_,    1L
    ),
    card_pool_set = tibble::tribble(
      ~card_pool_id, ~card_set_id,
      "pool_old",    "set_a",
      "pool_now",    "set_a",
      "pool_now",    "set_b",
      "pool_start",  "set_b",
      "pool_core",   "set_a"
    ),
    restriction_card = tibble::tribble(
      ~restriction_id, ~card_id, ~is_banned,  ~is_restricted, ~universal_faction_cost, ~global_penalty, ~points,
      "ban_now",       "ban_me", 1L,          NA_integer_,    NA_integer_,             NA_integer_,     NA_integer_,
      "ban_now",       "res_me", NA_integer_, 1L,             NA_integer_,             NA_integer_,     NA_integer_,
      "ban_now",       "both",   NA_integer_, NA_integer_,    2L,                      1L,              NA_integer_,
      "ban_now",       "pointy", NA_integer_, NA_integer_,    NA_integer_,             NA_integer_,     3L,
      "ban_old",       "clean",  1L,          NA_integer_,    NA_integer_,             NA_integer_,     NA_integer_
    ),
    printing = tibble::tribble(
      ~code,   ~card_id, ~card_set_id,
      "c_ban", "ban_me", "set_a",
      "c_res", "res_me", "set_a",
      "c_bth", "both",   "set_b",
      "c_pnt", "pointy", "set_b",
      "c_cln", "clean",  "set_a",
      "c_out", "outside","set_z"
    ),
    card_set = tibble::tribble(
      ~id,     ~legacy_code,
      "set_a", "pk_a",
      "set_b", "pk_b",
      "set_z", "pk_z"
    ),
    cards = tibble::tribble(
      ~code,   ~title,       ~pack_code,
      "c_ban", "Banned",     "pk_a",
      "c_res", "Restricted", "pk_a",
      "c_bth", "Both",       "pk_b",
      "c_pnt", "Pointy",     "pk_b",
      "c_cln", "Clean",      "pk_a",
      "c_out", "Outside",    "pk_z"
    )
  )
}

test_that("active_snapshot() picks the most recent snapshot already in force", {
  f <- format_fixture()
  expect_equal(active_snapshot(f$format_snapshot, "standard", as.Date("2026-08-26"))$id, "std_now")
  expect_equal(active_snapshot(f$format_snapshot, "standard", as.Date("2025-01-01"))$id, "std_old")
})

test_that("active_snapshot() goes by date, not upstream's lagging active flag", {
  # The regression this whole table exists to prevent: upstream marks
  # std_next active while std_now is the one actually in force.
  f <- format_fixture()
  snap <- active_snapshot(f$format_snapshot, "standard", as.Date("2026-08-26"))
  expect_equal(snap$id, "std_now")
  expect_equal(snap$is_active, 0L)
})

test_that("active_snapshot() returns NULL for an empty table or a format not yet started", {
  f <- format_fixture()
  expect_null(active_snapshot(f$format_snapshot[0, ], "standard"))
  expect_null(active_snapshot(f$format_snapshot, "standard", as.Date("2000-01-01")))
  expect_null(active_snapshot(f$format_snapshot, "eternal", as.Date("2026-08-26")))
})

test_that("annotate_format_legality() reads card-pool membership positively", {
  f <- format_fixture()
  ann <- annotate_format_legality(
    f$cards, active_snapshot(f$format_snapshot, "standard", as.Date("2026-08-26")),
    f$card_pool_set, f$restriction_card, f$printing, f$card_set
  )
  # set_z is in no pool, so Outside is out; everything else is in.
  expect_equal(ann$title[!ann$in_rotation], "Outside")
})

test_that("annotate_format_legality() applies only the snapshot's own restriction", {
  f <- format_fixture()
  ann <- annotate_format_legality(
    f$cards, active_snapshot(f$format_snapshot, "standard", as.Date("2026-08-26")),
    f$card_pool_set, f$restriction_card, f$printing, f$card_set
  )
  expect_equal(ann$title[ann$is_banned], "Banned")
  expect_equal(ann$title[ann$is_restricted], "Restricted")
  # "clean" is banned by ban_old, which this snapshot does not name.
  expect_false(ann$is_banned[ann$title == "Clean"])
})

test_that("annotate_format_legality() carries a card listed under several fields at once", {
  # Upstream can put one card in both universal_faction_cost and
  # global_penalty within a single list; the columns must not overwrite
  # one another.
  f <- format_fixture()
  ann <- annotate_format_legality(
    f$cards, active_snapshot(f$format_snapshot, "standard", as.Date("2026-08-26")),
    f$card_pool_set, f$restriction_card, f$printing, f$card_set
  )
  row <- ann[ann$title == "Both", ]
  expect_equal(row$influence_penalty, 2L)
  expect_equal(row$global_penalty, 1L)
  expect_equal(ann$points[ann$title == "Pointy"], 3L)
})

test_that("annotate_format_legality() handles a snapshot with a pool and no ban list", {
  f <- format_fixture()
  ann <- annotate_format_legality(
    f$cards, active_snapshot(f$format_snapshot, "core", as.Date("2026-08-26")),
    f$card_pool_set, f$restriction_card, f$printing, f$card_set
  )
  expect_false(any(ann$is_banned))
  expect_equal(sort(ann$title[ann$in_rotation]), sort(c("Banned", "Restricted", "Clean")))
})

test_that("annotate_format_legality() treats a NULL snapshot as no format applying", {
  f <- format_fixture()
  ann <- annotate_format_legality(f$cards, NULL, f$card_pool_set, f$restriction_card,
                                  f$printing, f$card_set)
  expect_true(all(ann$in_rotation))
  expect_false(any(ann$is_banned))
  expect_true(all(ann$points == 0L))
})

test_that("annotate_format_legality() falls back to pack_code when a printing row is missing", {
  # Exercised deliberately: the fallback never runs against real upstream
  # data, so only a fixture with printings removed proves it works. An
  # earlier version matched card codes against pack codes here and put
  # every card out of pool.
  f <- format_fixture()
  snap <- active_snapshot(f$format_snapshot, "standard", as.Date("2026-08-26"))
  full <- annotate_format_legality(f$cards, snap, f$card_pool_set, f$restriction_card,
                                   f$printing, f$card_set)
  none <- annotate_format_legality(f$cards, snap, f$card_pool_set, f$restriction_card,
                                   f$printing[0, ], f$card_set)
  expect_identical(none$in_rotation, full$in_rotation)
})

test_that("filter_legal() works against the v2 path's columns unchanged", {
  f <- format_fixture()
  ann <- annotate_format_legality(
    f$cards, active_snapshot(f$format_snapshot, "standard", as.Date("2026-08-26")),
    f$card_pool_set, f$restriction_card, f$printing, f$card_set
  )
  expect_equal(sort(filter_legal(ann)$title), sort(c("Restricted", "Both", "Pointy", "Clean")))
})

# ---- v2 readers --------------------------------------------------------

write_json_file <- function(dir, name, x) {
  fs::dir_create(dir)
  path <- file.path(dir, name)
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE), path)
  path
}

test_that("read_formats() flattens snapshots and keeps a missing restriction as NA", {
  dir <- withr::local_tempdir()
  write_json_file(dir, "standard.json", list(
    id = "standard", name = "Standard",
    snapshots = list(
      list(id = "s0", date_start = "2024-01-01", card_pool_id = "p0", restriction_id = "r0"),
      list(id = "s1", date_start = "2026-01-01", card_pool_id = "p1", active = TRUE)
    )
  ))
  out <- read_formats(dir)
  expect_equal(out$format$id, "standard")
  expect_equal(out$format_snapshot$id, c("s0", "s1"))
  expect_identical(out$format_snapshot$restriction_id, c("r0", NA_character_))
  expect_identical(out$format_snapshot$is_active, c(0L, 1L))
})

test_that("read_formats() returns empty tables for a tree with no v2 directory", {
  out <- read_formats(file.path(withr::local_tempdir(), "absent"))
  expect_equal(nrow(out$format), 0)
  expect_equal(nrow(out$format_snapshot), 0)
  expect_identical(names(out$format_snapshot),
                   c("id", "format_id", "date_start", "card_pool_id", "restriction_id", "is_active"))
})

test_that("read_card_pools() flattens each pool's set and cycle arrays", {
  dir <- withr::local_tempdir()
  write_json_file(dir, "standard.json", list(
    list(id = "p1", name = "Pool One", format_id = "standard",
         card_set_ids = c("set_b", "set_a"), card_cycle_ids = "cyc_a"),
    list(id = "p2", name = "Pool Two", format_id = "standard",
         card_set_ids = character(0), card_cycle_ids = character(0))
  ))
  out <- read_card_pools(dir)
  expect_equal(out$card_pool$id, c("p1", "p2"))
  expect_equal(out$card_pool_set$card_set_id, c("set_a", "set_b"))
  expect_equal(out$card_pool_cycle$card_cycle_id, "cyc_a")
})

test_that("read_restrictions() inverts the value-keyed objects into card rows", {
  # universal_faction_cost / global_penalty / points are keyed BY the
  # value with an array of card ids under it -- the names are numbers,
  # not card ids. Getting this backwards is the failure mode.
  dir <- file.path(withr::local_tempdir(), "standard")
  write_json_file(dir, "r1.json", list(
    id = "r1", name = "R One", format_id = "standard", date_start = "2026-01-01",
    banned = c("b1", "b2"), restricted = "s1",
    universal_faction_cost = list(`3` = c("u1", "shared")),
    global_penalty = list(`1` = "shared"),
    points = list(`2` = "p1"),
    point_limit = 7, max_3_point_agendas = 1,
    subtypes = list(banned = "current")
  ))
  out <- read_restrictions(dirname(dir))

  expect_equal(out$restriction$format_id, "standard")
  expect_equal(out$restriction$point_limit, 7L)
  expect_equal(out$restriction$max_3_point_agendas, 1L)

  expect_equal(sort(out$restriction_card$card_id), c("b1", "b2", "p1", "s1", "shared", "u1"))
  expect_equal(out$restriction_card$is_banned[out$restriction_card$card_id == "b1"], 1L)
  expect_equal(out$restriction_card$is_restricted[out$restriction_card$card_id == "s1"], 1L)
  expect_equal(out$restriction_card$points[out$restriction_card$card_id == "p1"], 2L)

  # One card under two value-keyed fields stays one row carrying both.
  shared <- out$restriction_card[out$restriction_card$card_id == "shared", ]
  expect_equal(nrow(shared), 1)
  expect_equal(shared$universal_faction_cost, 3L)
  expect_equal(shared$global_penalty, 1L)

  expect_equal(out$restriction_subtype$subtype_id, "current")
  expect_equal(out$restriction_subtype$kind, "banned")
})

test_that("read_restrictions() takes the declared format_id, not the directory name", {
  # Upstream files under standard/ carry startup_-prefixed names while
  # declaring format_id "standard"; the directory must not win.
  dir <- file.path(withr::local_tempdir(), "standard")
  write_json_file(dir, "startup_ban_list_24_01.json", list(
    id = "startup_ban_list_24_01_for_classic_only", name = "Startup Ban List",
    format_id = "standard", date_start = "2023-09-21", banned = "x"
  ))
  out <- read_restrictions(dirname(dir))
  expect_equal(out$restriction$format_id, "standard")
})

test_that("read_restrictions() keeps all five card columns when a list uses only one", {
  dir <- file.path(withr::local_tempdir(), "standard")
  write_json_file(dir, "r1.json", list(
    id = "r1", name = "R One", format_id = "standard", date_start = "2026-01-01",
    banned = "b1"
  ))
  out <- read_restrictions(dirname(dir))
  expect_identical(
    names(out$restriction_card),
    c("restriction_id", "card_id", "is_banned", "is_restricted",
      "universal_faction_cost", "global_penalty", "points")
  )
  expect_identical(out$restriction_card$points, NA_integer_)
})

test_that("read_printings() and read_card_sets() give the bridge to v2 card ids", {
  dir <- withr::local_tempdir()
  write_json_file(file.path(dir, "printings"), "core_set.json", list(
    list(id = "01002", card_id = "kakugo", card_set_id = "core_set"),
    list(id = "01001", card_id = "noise", card_set_id = "core_set")
  ))
  sets_path <- write_json_file(dir, "card_sets.json", list(
    list(id = "core_set", name = "Core Set", legacy_code = "core",
         card_cycle_id = "core_set", date_release = "2012-09-06", position = 1)
  ))
  printings <- read_printings(file.path(dir, "printings"))
  expect_equal(printings$code, c("01001", "01002"))
  expect_equal(printings$card_id, c("noise", "kakugo"))
  expect_equal(read_card_sets(sets_path)$legacy_code, "core")
})

test_that("mwl_v2_format() resolves legacy codes across both separator conventions", {
  restriction <- tibble::tribble(
    ~id,                         ~format_id,
    "napd_mwl_2_1",              "standard",
    "standard_ban_list_26_03",   "standard",
    "startup_ban_list_24_01",    "startup"
  )
  expect_equal(
    mwl_v2_format(c("NAPD_MWL_2.1", "standard-ban-list-26-03", "startup-ban-list-24-01"), restriction),
    c("standard", "standard", "startup")
  )
})

test_that("mwl_v2_format() returns NA rather than guessing when the trees diverge", {
  restriction <- tibble::tibble(id = "standard_ban_list_26_03", format_id = "standard")
  expect_identical(mwl_v2_format("brandnew-list-99", restriction), NA_character_)
  expect_identical(mwl_v2_format("standard-ban-list-26-03", restriction[0, ]), NA_character_)
})

test_that("the prefix a legacy code carries is NOT its format", {
  # The defect this PR removes, pinned so nobody reintroduces the guess:
  # every one of these codes declares "standard" upstream despite a
  # prefix saying otherwise.
  restriction <- tibble::tribble(
    ~id,                                             ~format_id,
    "napd_mwl_2_1",                                  "standard",
    "sunset_ban_list_24_01",                         "standard",
    "startup_balance_update_26_05_for_classic_only", "standard"
  )
  misleading <- c("NAPD_MWL_2.1", "sunset-ban-list-24-01",
                  "startup-balance-update-26-05-for-classic-only")
  expect_equal(mwl_v2_format(misleading, restriction), rep("standard", 3))
  expect_false(any(tolower(sub("[-_].*$", "", misleading)) == "standard"))
})
