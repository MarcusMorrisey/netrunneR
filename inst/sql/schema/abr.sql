-- ABR lineage processed schema. Columns here are exactly the
-- ABR_TOURNAMENT_ALLOWLIST set from R/build-abr.R -- no personal-data
-- column (player/organizer name, contact, address) is ever defined in
-- this table.
CREATE TABLE tournament (
  id TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  date TEXT NOT NULL,
  format TEXT NOT NULL,
  location_state TEXT,
  location_country TEXT,
  players_count INTEGER,
  top_count INTEGER,
  winner_runner_identity TEXT,
  winner_corp_identity TEXT
);
