-- ABR lineage processed schema. Columns here are exactly the
-- ABR_TOURNAMENT_ALLOWLIST set from R/build-abr.R -- no personal-data
-- column (player/organizer name, contact, address) is ever defined in
-- this table.
CREATE TABLE tournament (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  date TEXT NOT NULL,
  format TEXT NOT NULL,
  region TEXT,
  country TEXT,
  player_count INTEGER,
  cut_size INTEGER,
  identity_a_code TEXT,
  identity_c_code TEXT
);
