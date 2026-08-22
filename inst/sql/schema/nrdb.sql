-- NetrunnerDB lineage processed schema: decklist, review and ruling
-- tables populated by R/build-nrdb.R from the date-range decklist
-- sweep and the reviews/rulings endpoints.
CREATE TABLE decklist (
  id TEXT PRIMARY KEY,
  date TEXT NOT NULL,
  name TEXT,
  identity_code TEXT,
  cards TEXT,
  status TEXT NOT NULL DEFAULT 'active'
);

CREATE TABLE review (
  id TEXT PRIMARY KEY,
  card_code TEXT,
  text TEXT,
  created_at TEXT
);

CREATE TABLE ruling (
  id TEXT PRIMARY KEY,
  card_code TEXT,
  text TEXT,
  created_at TEXT
);
