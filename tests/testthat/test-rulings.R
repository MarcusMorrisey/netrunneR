# Shapes copied from the real nrdb `ruling` table: Markdown bodies with
# inline NetrunnerDB links and a trailing bracketed source marker, plus
# the nsg_rules_team_verified flag.

mini_rulings <- function() {
  tibble::tribble(
    ~title,        ~ruling,                                                          ~date_update,  ~nsg_rules_team_verified,
    "Cheap Wall",  "Cards trashed this way go to Archives facedown. [Official FAQ]", "2017-04-08",  0L,
    "Cheap Wall",  "Can [Cheap Wall](https://netrunnerdb.com/en/card/01001) be advanced?\n\n> Yes. [NSG Rules Team Update]", "2021-06-01", 1L,
    "Tall Wall",   "Strength is now 4, was 5. [Uprising Release Notes]",             "2020-03-12",  0L
  )
}

test_that("a card's rulings come back newest first", {
  hits <- rulings_for_card(mini_rulings(), "Cheap Wall")
  expect_equal(nrow(hits), 2L)
  expect_equal(hits$date_update[1], "2021-06-01")
})

test_that("a card with no rulings gets an empty frame, not an error", {
  expect_equal(nrow(rulings_for_card(mini_rulings(), "Nonexistent")), 0L)
})

test_that("a missing nrdb release is survivable", {
  # Rulings are an augmentation; the app is about ice/breaker economics
  # and must still open without them.
  expect_equal(nrow(rulings_for_card(NULL, "Cheap Wall")), 0L)
})

test_that("the source marker is read from the end, not the first bracket", {
  # The body contains a Markdown link whose label is also bracketed. Taking
  # the first bracket would report the card's own name as the ruling's
  # source, which is exactly backwards.
  labels <- ruling_source_label(mini_rulings()$ruling)
  expect_equal(labels, c("Official FAQ", "NSG Rules Team Update", "Uprising Release Notes"))
})

test_that("a ruling with no marker yields NA rather than a wrong source", {
  expect_true(is.na(ruling_source_label("Just some text with no marker.")))
})

test_that("ruling Markdown is rendered, not shown raw", {
  html <- as.character(ruling_body_html("Can [X](https://netrunnerdb.com/en/card/01001) be advanced?\n\n> Yes."))
  expect_match(html, "<a href")
  expect_match(html, "<blockquote>")
  expect_no_match(html, "](https", fixed = TRUE)
})

test_that("embedded HTML in ruling text is stripped, not trusted", {
  # Third-party text arriving over the network into the page. The mirror
  # is offline; the content in it was not authored here.
  html <- as.character(ruling_body_html("Careful <script>alert(1)</script> now"))
  expect_no_match(html, "<script", fixed = TRUE)
})

test_that("the rulings panel is withheld while the nrdb gate is closed", {
  # A closed gate means the feature is not shipped, which must not abort
  # the card-detail modal wrapped around it.
  withr::local_options(list())
  local_mocked_bindings(NRDB_ATTRIBUTION_CONFIRMED = FALSE)
  expect_null(card_rulings_ui(mini_rulings(), "Cheap Wall"))
})

test_that("with the gate open the panel renders the rulings and the disclaimer", {
  local_mocked_bindings(NRDB_ATTRIBUTION_CONFIRMED = TRUE)
  rendered <- as.character(shiny::tagList(card_rulings_ui(mini_rulings(), "Cheap Wall")))

  expect_match(rendered, "Rulings (2)", fixed = TRUE)
  expect_match(rendered, "Official FAQ")
  expect_match(rendered, "RULES TEAM VERIFIED")
  # The guard is only meaningful if the disclaimer is actually there.
  expect_match(rendered, "not produced, endorsed, supported, or affiliated")
  expect_match(rendered, "netrunnerdb.com")
})

test_that("an unruled card renders no panel at all, claiming nothing", {
  # An empty panel headed "Rulings" would read as "this card has none",
  # which the mirror cannot promise -- it holds what NetrunnerDB had at
  # sync time.
  local_mocked_bindings(NRDB_ATTRIBUTION_CONFIRMED = TRUE)
  expect_null(card_rulings_ui(mini_rulings(), "Nonexistent"))
})

test_that("errata from release notes are carried like any other ruling", {
  local_mocked_bindings(NRDB_ATTRIBUTION_CONFIRMED = TRUE)
  rendered <- as.character(shiny::tagList(card_rulings_ui(mini_rulings(), "Tall Wall")))
  expect_match(rendered, "Uprising Release Notes")
  expect_match(rendered, "Strength is now 4")
})

test_that("user reviews are not part of the rulings panel", {
  # The same lineage mirrors 4,265 reviews. Those are opinion about
  # whether a card is good, not rules about how it works.
  expect_false(any(grepl("review", names(mini_rulings()), ignore.case = TRUE)))
})
