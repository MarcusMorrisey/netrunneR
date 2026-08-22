-- Relational schema for the cardpool lineage (ref: DL-004: git-mirror
-- source, no network/personal-data handling). Applied fresh to each
-- build's SQLite file by apply_schema() before data is written.
--
CREATE TABLE cycle (
  code TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  position INTEGER NOT NULL
);

CREATE TABLE faction (
  code TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  side TEXT NOT NULL
);

CREATE TABLE pack (
  code TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  cycle_code TEXT NOT NULL REFERENCES cycle(code),
  position INTEGER NOT NULL
);

CREATE TABLE card (
  code TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  pack_code TEXT NOT NULL REFERENCES pack(code),
  faction_code TEXT NOT NULL REFERENCES faction(code),
  type_code TEXT NOT NULL,
  side_code TEXT NOT NULL,
  text TEXT,
  cost INTEGER,
  strength INTEGER,
  keywords TEXT
);
