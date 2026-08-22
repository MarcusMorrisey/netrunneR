-- Schema for the implementation lineage: normalized ice/breaker trait rows
-- keyed by the same `code` cardpool uses (cross-checked, not enforced by an
-- FK, since implementation and cardpool are fetched independently).
CREATE TABLE ice_breaker_traits (
  code TEXT PRIMARY KEY,
  subtypes TEXT,
  base_strength INTEGER,
  break_cost INTEGER
);
