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

-- `format` is not in mwl.json, which carries no format field. It is
-- resolved by joining each entry's code to the matching v2 restriction
-- id (see restriction below and mwl_v2_format() in R/build-cardpool.R),
-- which declares its format outright. It was previously guessed from
-- the code prefix; that guess was wrong for 13 of the 41 entries.
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

-- ---------------------------------------------------------------------
-- Authoritative format / card-pool / restriction model, from the same
-- mirrored repo's v2/ tree.
--
-- The tables above (rotation, mwl) come from the legacy top-level
-- rotations.json and mwl.json, which carry no format field at all. The
-- v2 tree states it outright: every restriction declares its own
-- `format_id`, and every format lists ordered snapshots binding a date
-- to a card pool and a restriction. The two disagree for 13 of the 41
-- legacy ban lists -- all six NAPD_MWL_*, sunset-ban-list-24-01 and all
-- six *-for-classic-only entries are Standard, not the separate
-- "napd" / "sunset" / "startup" formats a code prefix implies -- so
-- these tables, not the prefix, are the source of truth. `mwl.format`
-- is now populated by joining to restriction.format_id.

CREATE TABLE format (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL
);

-- Card sets are v2's name for what the legacy files call packs, and
-- `legacy_code` is exactly the v1 pack code (verified: the two sets
-- correspond 1:1 upstream, in both directions). Carried so a card pool,
-- which lists card_set_ids, can be resolved to the pack codes the card
-- table actually uses.
CREATE TABLE card_set (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  legacy_code TEXT NOT NULL,
  card_cycle_id TEXT NOT NULL,
  date_release TEXT,
  position INTEGER
);

-- v2 keys cards by a stable slug (`kakugo`) while the card table is
-- keyed by printing code (`11001`). Restrictions and card pools speak
-- slugs, so this bridge table is what makes them joinable to card.
CREATE TABLE printing (
  code TEXT PRIMARY KEY,
  card_id TEXT NOT NULL,
  card_set_id TEXT NOT NULL REFERENCES card_set(id)
);

-- A card pool states its contents POSITIVELY (the sets and cycles that
-- are in), unlike rotation_cycle, which lists what rotated OUT and
-- leaves membership to be inferred.
CREATE TABLE card_pool (
  id TEXT PRIMARY KEY,
  format_id TEXT NOT NULL REFERENCES format(id),
  name TEXT NOT NULL
);

CREATE TABLE card_pool_set (
  card_pool_id TEXT NOT NULL REFERENCES card_pool(id),
  card_set_id TEXT NOT NULL REFERENCES card_set(id),
  PRIMARY KEY (card_pool_id, card_set_id)
);

CREATE TABLE card_pool_cycle (
  card_pool_id TEXT NOT NULL REFERENCES card_pool(id),
  card_cycle_id TEXT NOT NULL,
  PRIMARY KEY (card_pool_id, card_cycle_id)
);

-- `restriction_id` is nullable: core, system_gateway and ram snapshots
-- have a card pool but no ban list at all.
--
-- `is_active` is upstream's own `active` flag, stored but NOT trusted as
-- the selector -- at the mirrored commit it still marked standard_34
-- (2026-03-13) active while standard_35 and standard_36 had already
-- started, so it is hand-maintained and lags. active_snapshot() goes by
-- date; the build's snapshot_active_flag check reports the disagreement
-- rather than silently preferring either one.
CREATE TABLE format_snapshot (
  id TEXT PRIMARY KEY,
  format_id TEXT NOT NULL REFERENCES format(id),
  date_start TEXT NOT NULL,
  card_pool_id TEXT NOT NULL REFERENCES card_pool(id),
  restriction_id TEXT REFERENCES restriction(id),
  is_active INTEGER NOT NULL
);

CREATE TABLE restriction (
  id TEXT PRIMARY KEY,
  name TEXT NOT NULL,
  format_id TEXT NOT NULL REFERENCES format(id),
  date_start TEXT NOT NULL,
  point_limit INTEGER,
  max_3_point_agendas INTEGER
);

-- One row per (restriction, card). Upstream splits these across five
-- differently shaped fields -- `banned` and `restricted` are arrays of
-- card ids, while `universal_faction_cost`, `global_penalty` and
-- `points` are OBJECTS keyed by the numeric value with an array of card
-- ids as the value -- and a single card can appear under more than one
-- within one list, so they are merged into columns here rather than
-- stored as (kind, value) rows.
CREATE TABLE restriction_card (
  restriction_id TEXT NOT NULL REFERENCES restriction(id),
  card_id TEXT NOT NULL,
  is_banned INTEGER,
  is_restricted INTEGER,
  universal_faction_cost INTEGER,
  global_penalty INTEGER,
  points INTEGER,
  PRIMARY KEY (restriction_id, card_id)
);

-- Some lists ban a whole SUBTYPE rather than named cards
-- (standard_ban_list_20_06 bans `current`). Dropping this field would
-- silently under-report what a list actually forbids.
CREATE TABLE restriction_subtype (
  restriction_id TEXT NOT NULL REFERENCES restriction(id),
  subtype_id TEXT NOT NULL,
  kind TEXT NOT NULL,
  PRIMARY KEY (restriction_id, subtype_id, kind)
);
