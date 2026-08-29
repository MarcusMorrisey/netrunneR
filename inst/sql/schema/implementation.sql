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
  -- How many DISTINCT subtypes this card's break clauses name. Above 1,
  -- break_subtype above is known to be incomplete -- Penrose breaks Code
  -- Gates and Barriers, Lobisomem breaks Code Gates and Barriers, and
  -- only one of each pair is stored. Recorded so a consumer can decline
  -- to say "cannot break" on evidence it knows is partial, rather than
  -- reporting our own dropped clause as a fact about the card.
  break_subtype_count INTEGER,
  -- Credits only. Stealth credits ARE credits -- the qualifier says where
  -- they must be sourced, not that they are a different currency -- so
  -- they are counted here and the constraint is recorded in
  -- pump_stealth. Power/virus counters and trashing a card are NOT
  -- credits and are deliberately kept out of this column: folding them
  -- in would require a conversion rate that does not exist.
  pump_cost INTEGER,
  pump_amount INTEGER,
  -- How many of pump_cost's credits must come from a stealth card.
  -- NULL means no such constraint, not zero.
  pump_stealth INTEGER,
  -- A non-credit pump cost: 'power', 'virus', 'trash-from-hand',
  -- 'trash-can', 'trash-installed', 'any-virus-counter'. NULL for the
  -- great majority, which pay credits.
  pump_resource_type TEXT,
  pump_resource_qty INTEGER,
  -- Why any NULL above is NULL: 'parsed', 'parsed_no_pump',
  -- 'variable_subroutines', 'non_credit_break_cost', 'unreadable_form'.
  -- Recorded rather than inferred, so a gap is reportable instead of
  -- indistinguishable from a card nobody has looked at.
  parse_status TEXT NOT NULL
);
