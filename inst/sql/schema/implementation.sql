-- Schema for the implementation lineage.
--
-- v2 REPLACES v1 ENTIRELY. v1 stored subtypes and base_strength per card,
-- neither of which exists in the mtgred/netrunner tree: that tree defines
-- card BEHAVIOUR, while subtypes, strength and cost come from cardpool.
-- Storing them here made a second, staler copy of data the app already
-- had, and left the columns NULL in practice. What only the
-- implementation knows is the economics -- how many subroutines a piece
-- of ice has, and what a breaker charges to pump and to break.
--
-- Keyed by the same `code` cardpool uses (cross-checked, not enforced by
-- an FK, since implementation and cardpool are fetched independently).
-- A defcard names a card, so one title expands to every printing's code.
CREATE TABLE ice_breaker_traits (
  code TEXT PRIMARY KEY,
  title TEXT NOT NULL,
  -- 'ice' or 'program'. Which columns below are meaningful depends on it.
  kind TEXT NOT NULL,
  -- ice only. NULL where the count is genuinely variable (Ashigaru counts
  -- cards in HQ) or the form could not be read.
  subroutine_count INTEGER,
  -- breaker only, all NULL together. break_qty 0 means 'break all
  -- subroutines at once'.
  break_cost INTEGER,
  break_qty INTEGER,
  -- 'Barrier' / 'Code Gate' / 'Sentry' / 'All' for an AI breaker.
  break_subtype TEXT,
  pump_cost INTEGER,
  pump_amount INTEGER,
  -- Why any NULL above is NULL: 'parsed', 'parsed_no_pump',
  -- 'variable_subroutines', 'non_credit_break_cost', 'unreadable_form'.
  -- Recorded rather than inferred, so a gap is reportable instead of
  -- indistinguishable from a card nobody has looked at.
  parse_status TEXT NOT NULL
);
