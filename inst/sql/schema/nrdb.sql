-- NetrunnerDB lineage processed schema: review and ruling tables
-- populated by R/build-nrdb.R from the reviews/rulings endpoints.
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
