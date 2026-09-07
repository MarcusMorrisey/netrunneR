-- Cobra lineage processed schema. Ten tables built by R/build-cobra.R
-- from every resolved bundle in the content-addressed object pool
-- (R/fetch-cobra.R). Columns here are exactly the COBRA_*_ALLOWLIST
-- sets in R/build-cobra.R -- no personal-data column (player display
-- name, name_with_pronouns, contact, address) is ever defined in any
-- table here. Unlike abr.sql, no table admits a coordinate: Cobra's
-- tournament records carry no venue-coordinate field at all.
CREATE TABLE tournament (
  tournament_id TEXT PRIMARY KEY,
  name TEXT,
  slug TEXT,
  -- abr_code: NOT a reference to abr.tournament.id -- corrected
  -- 2026-09-05, see COBRA_TOURNAMENT_ALLOWLIST (R/build-cobra.R) for the
  -- full correction. Upstream-supplied, nullable, but its value space
  -- (six digits) does not overlap abr.tournament.id's (1-5842); do not
  -- join on it. What it actually references is unconfirmed.
  abr_code TEXT,
  private INTEGER,
  date TEXT,
  time_zone TEXT,
  registration_starts TEXT,
  tournament_starts TEXT,
  decklist_required INTEGER,
  self_registration INTEGER,
  allow_self_reporting INTEGER,
  swiss_deck_visibility TEXT,
  cut_deck_visibility TEXT,
  swiss_format TEXT,
  tournament_type_id INTEGER,
  format_id INTEGER,
  deckbuilding_restriction_id TEXT,
  card_set_id TEXT,
  created_at TEXT,
  updated_at TEXT,
  policy_update INTEGER,
  custom_table_numbering INTEGER,
  stage_count INTEGER,
  round_count INTEGER,
  pairing_count INTEGER
);

CREATE TABLE stage (
  tournament_id TEXT NOT NULL REFERENCES tournament(tournament_id),
  stage_id INTEGER,
  stage_name TEXT,
  format TEXT,
  is_single_sided INTEGER,
  is_elimination INTEGER,
  view_decks INTEGER,
  player_count INTEGER
);

CREATE TABLE round (
  tournament_id TEXT NOT NULL REFERENCES tournament(tournament_id),
  stage_id INTEGER,
  stage_name TEXT,
  stage_format TEXT,
  round_id INTEGER,
  round_number INTEGER,
  completed INTEGER,
  pairings_reported INTEGER,
  length_minutes INTEGER,
  timer_running INTEGER,
  timer_paused INTEGER,
  timer_started INTEGER,
  pairing_count INTEGER
);

-- corp_identity/runner_identity name a game identity, not a person, same
-- precedent as ABR_TOURNAMENT_ALLOWLIST's winner_runner_identity/
-- winner_corp_identity (inst/sql/schema/abr.sql).
CREATE TABLE pairing (
  tournament_id TEXT NOT NULL REFERENCES tournament(tournament_id),
  stage_id INTEGER,
  stage_name TEXT,
  stage_format TEXT,
  round_id INTEGER,
  round_number INTEGER,
  pairing_id INTEGER,
  table_number INTEGER,
  table_label TEXT,
  reported INTEGER,
  intentional_draw INTEGER,
  two_for_one INTEGER,
  score1 REAL,
  score2 REAL,
  score_label TEXT,
  player1_id INTEGER,
  player1_seed INTEGER,
  player1_side TEXT,
  player1_corp_identity TEXT,
  player1_corp_faction TEXT,
  player1_runner_identity TEXT,
  player1_runner_faction TEXT,
  player2_id INTEGER,
  player2_seed INTEGER,
  player2_side TEXT,
  player2_corp_identity TEXT,
  player2_corp_faction TEXT,
  player2_runner_identity TEXT,
  player2_runner_faction TEXT
);

CREATE TABLE standing (
  tournament_id TEXT NOT NULL REFERENCES tournament(tournament_id),
  stage_format TEXT,
  rounds_complete INTEGER,
  any_decks_viewable INTEGER,
  position INTEGER,
  player_id INTEGER,
  active INTEGER,
  corp_identity TEXT,
  corp_faction TEXT,
  runner_identity TEXT,
  runner_faction TEXT,
  points REAL,
  sos REAL,
  extended_sos REAL,
  corp_points REAL,
  runner_points REAL,
  bye_points REAL,
  seed INTEGER,
  manual_seed INTEGER,
  side_bias REAL,
  view_decks INTEGER
);

CREATE TABLE faction_count (
  tournament_id TEXT NOT NULL REFERENCES tournament(tournament_id),
  scope TEXT,
  side TEXT,
  faction TEXT,
  count INTEGER,
  num_players INTEGER
);

CREATE TABLE identity_count (
  tournament_id TEXT NOT NULL REFERENCES tournament(tournament_id),
  scope TEXT,
  side TEXT,
  identity_name TEXT,
  faction TEXT,
  count INTEGER,
  num_players INTEGER
);

CREATE TABLE cut_conversion_faction (
  tournament_id TEXT NOT NULL REFERENCES tournament(tournament_id),
  side TEXT,
  faction TEXT,
  num_swiss_players INTEGER,
  num_cut_players INTEGER,
  cut_conversion_percentage REAL
);

CREATE TABLE cut_conversion_identity (
  tournament_id TEXT NOT NULL REFERENCES tournament(tournament_id),
  side TEXT,
  identity_name TEXT,
  faction TEXT,
  num_swiss_players INTEGER,
  num_cut_players INTEGER,
  cut_conversion_percentage REAL
);

-- Discovery provenance from discover_cobra_recent_index() (R/fetch-cobra.R):
-- which of the 12 public type-listing pages a tournament id was seen on,
-- and when. Not tournament data proper, but kept alongside it since it is
-- the only record of how an id not found via the historical backfill walk
-- was actually discovered.
CREATE TABLE recent_index (
  tournament_id INTEGER,
  tournament_type_id INTEGER,
  discovered_at TEXT
);

-- Merged abr+cobra tournament feed (R/merge-abr-cobra.R), built alongside
-- the ten cobra-only tables above. Column set is exactly abr.tournament's
-- (inst/sql/schema/abr.sql) so SELECT * FROM tournament_merged and
-- SELECT * FROM tournament (abr) are byte-identical in shape -- the app's
-- read-side fallback between the two must never need a reshape. id holds
-- the synthetic merged key ("abr:<id>" / "cobra:<id>" /
-- "abr:<id>+cobra:<id>"); there is no separate merged_id column here
-- because no app consumer reads tournament$id, so the id column was free
-- to repurpose. (DL-049, DL-053)
CREATE TABLE tournament_merged (
  id TEXT PRIMARY KEY,
  title TEXT,
  date TEXT,
  format TEXT,
  type TEXT,
  location_state TEXT,
  location_country TEXT,
  location_lat REAL,
  location_lng REAL,
  players_count INTEGER,
  top_count INTEGER,
  winner_runner_identity TEXT,
  winner_corp_identity TEXT
);

-- Provenance for tournament_merged: one row per (source, source_id) that
-- contributed to a merged row. A flat abr_id/cobra_tournament_id column
-- pair on tournament_merged could not represent a group match (several
-- cobra rows -- e.g. NISEI Worlds split across per-day rows -- summing
-- into one abr row, per abr_cobra_verified_group_matches.csv), so
-- provenance lives in its own one-to-many table instead. match_tier
-- records which override-precedence tier produced the pairing (deleted
-- cobra rows never appear here at all, in either table). (DL-050, DL-062)
CREATE TABLE tournament_merged_source (
  merged_id TEXT NOT NULL REFERENCES tournament_merged(id),
  source TEXT NOT NULL,      -- 'abr' | 'cobra'
  source_id TEXT NOT NULL,
  match_tier TEXT NOT NULL,  -- 'group' | 'verified' | 'algorithmic' | 'single'
  PRIMARY KEY (merged_id, source, source_id)
);
