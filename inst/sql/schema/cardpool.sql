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

-- Rotation and ban-list (MWL) legality, from the same upstream repo's
-- rotations.json and mwl.json. Both upstream files nest a collection
-- inside each record -- rotations.json a `rotated` array of cycle codes,
-- mwl.json a `cards` OBJECT keyed by card code -- so both are flattened
-- into the long child tables below rather than stored as a list-column.
-- That shape is the whole reason the removed decklist feature failed
-- (an unserializable nested `cards` list-column reaching dbWriteTable);
-- see R/README.md's "Decklist mirroring was removed, not repaired".

CREATE TABLE rotation (
  code TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  date_start TEXT NOT NULL
);

-- One row per cycle ROTATED OUT by a rotation (upstream's `rotated`
-- array is the exclusion list, not the legal list).
CREATE TABLE rotation_cycle (
  rotation_code TEXT NOT NULL REFERENCES rotation(code),
  cycle_code TEXT NOT NULL REFERENCES cycle(code),
  PRIMARY KEY (rotation_code, cycle_code)
);

-- `format` is DERIVED here, not upstream: mwl.json records carry no
-- format field, so it is taken from the code prefix (standard / startup
-- / napd / sunset -- see MWL_FORMAT_PREFIXES in R/build-cardpool.R).
-- Stored rather than re-derived per query so the derivation lives in
-- exactly one place.
CREATE TABLE mwl (
  code TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  format TEXT NOT NULL,
  date_start TEXT NOT NULL
);

-- One row per (list, card) restriction. Upstream gives each card exactly
-- one of these four keys, never a combination, so the other three are
-- NULL on any given row.
CREATE TABLE mwl_card (
  mwl_code TEXT NOT NULL REFERENCES mwl(code),
  card_code TEXT NOT NULL REFERENCES card(code),
  deck_limit INTEGER,
  is_restricted INTEGER,
  universal_faction_cost INTEGER,
  global_penalty INTEGER,
  PRIMARY KEY (mwl_code, card_code)
);
