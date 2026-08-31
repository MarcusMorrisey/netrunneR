-- ABR lineage processed schema. Columns here are exactly the
-- ABR_TOURNAMENT_ALLOWLIST set from R/build-abr.R -- no personal-data
-- column (player/organizer name, contact, address) is ever defined in
-- this table. Venue coordinates are not personal data and are defined
-- here deliberately; see the allowlist docstring.
CREATE TABLE tournament (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  date TEXT NOT NULL,
  format TEXT NOT NULL,
  -- The event's category (GNK, store championship, worlds, ...), not a
  -- person's anything. See ABR_TOURNAMENT_ALLOWLIST for why it is
  -- admitted. NULLABLE, unlike format: one upstream record carries a
  -- separator row from abr's own dropdown rather than a real type, and a
  -- NOT NULL here would make the whole build fail over one bad string.
  type TEXT,
  location_state TEXT,
  location_country TEXT,
  -- Venue coordinates: where the event was held, not where anyone lives.
  -- See ABR_TOURNAMENT_ALLOWLIST (R/build-abr.R) for why these are
  -- admitted while every organiser, player and contact field stays
  -- excluded. REAL rather than TEXT because the map arithmetic is
  -- numeric and the upstream API returns them as strings.
  location_lat REAL,
  location_lng REAL,
  players_count INTEGER,
  top_count INTEGER,
  winner_runner_identity TEXT,
  winner_corp_identity TEXT
);
