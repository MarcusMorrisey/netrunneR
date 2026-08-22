#' Compute Elo-style identity and faction ratings
#'
#' Runs the identity-elo-v1 spec over canonical game rows built from abr
#' match data, with initial rating 1500, K of 32, scores of 1 and 0 or 0.5
#' only on a source-recorded draw, no side bonus and no inactivity decay.
#' PlayerRatings::elo() fixes its expected-score divisor at 400 internally
#' (the standard Elo formula) and exposes no argument to configure it, so
#' the divisor requirement is satisfied by elo()'s own default behavior,
#' not by a passed argument. Rows order by tournament date UTC,
#' tournament id, source round sequence and row sha256 as final tie-break;
#' identity codes and faction codes are rated independently and emitted
#' in code order. Ratings apply to cards and factions only, never to
#' people.
#'
#' @param games A tibble of canonical game rows with columns identity_a,
#'
#'   identity_b, faction_a, faction_b, score_a, tournament_date,
#'   tournament_id, round_sequence.
#'
#' @return A list with `identity_ratings` and `faction_ratings` tibbles.
#' @export
compute_identity_ratings <- function(games) {
  games <- canonical_game_order(games)

  identity_ratings <- run_elo(games, games$identity_a, games$identity_b, games$score_a)
  faction_ratings <- run_elo(games, games$faction_a, games$faction_b, games$score_a)

  list(
    identity_ratings = identity_ratings[order(identity_ratings$code), ],
    faction_ratings = faction_ratings[order(faction_ratings$code), ]
  )
}

#' Order canonical game rows deterministically
#'
#' Orders by tournament date UTC, tournament id, source round sequence and
#' finally row sha256, so shuffling the input in memory before this call
#' always reproduces bit-for-bit identical rating output.
#' @keywords internal
canonical_game_order <- function(games) {
  games$row_sha256 <- purrr::pmap_chr(games, function(...) digest::digest(list(...), algo = "sha256"))
  dplyr::arrange(games, .data$tournament_date, .data$tournament_id, .data$round_sequence, .data$row_sha256)
}

#' Run an Elo rating pass with fixed identity-elo-v1 parameters
#' @keywords internal
run_elo <- function(games, side_a, side_b, score_a) {
  match_df <- data.frame(
    day = as.integer(games$tournament_date - min(games$tournament_date)) + 1,
    white = side_a,
    black = side_b,
    score = score_a
  )
  result <- PlayerRatings::elo(
    match_df, status = NULL, init = 1500, kfac = 32, gamma = 0, history = FALSE
  )
  tibble::tibble(code = result$ratings$Player, rating = result$ratings$Rating)
}
