test_that("compute_identity_ratings() emits identity and faction ratings ordered by code", {
# Covers R/views-ratings.R: PlayerRatings::elo()-based identity/faction
# ratings and the deterministic canonical_game_order() tie-break.

  games <- tibble::tibble(
    identity_a = c("id-1", "id-2"), identity_b = c("id-2", "id-1"),
    faction_a = c("anarch", "shaper"), faction_b = c("shaper", "anarch"),
    score_a = c(1, 0),
    tournament_date = as.Date(c("2023-01-01", "2023-01-02")),
    tournament_id = c("t1", "t2"),
    round_sequence = c(1L, 1L)
  )

  result <- compute_identity_ratings(games)

  expect_true(is.unsorted(result$identity_ratings$code) == FALSE)
  expect_setequal(result$identity_ratings$code, c("id-1", "id-2"))
  expect_setequal(result$faction_ratings$code, c("anarch", "shaper"))
})

test_that("canonical_game_order() is deterministic regardless of input row order", {
  games <- tibble::tibble(
    identity_a = c("a", "b"), identity_b = c("b", "a"),
    faction_a = c("x", "y"), faction_b = c("y", "x"),
    score_a = c(1, 0),
    tournament_date = as.Date(c("2023-01-01", "2023-01-01")),
    tournament_id = c("t1", "t1"),
    round_sequence = c(1L, 2L)
  )
  shuffled <- games[c(2, 1), ]

  ordered_a <- canonical_game_order(games)
  ordered_b <- canonical_game_order(shuffled)
  expect_identical(ordered_a$row_sha256, ordered_b$row_sha256)
})
