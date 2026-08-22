-- Schema for the rules lineage. `pooled_hash` is the sha256 of the PDF's
-- bytes at fetch time; build-rules.R's checks re-verify it against the
-- pooled object before promotion, never silently rewriting it.
CREATE TABLE rules_version (
  version TEXT PRIMARY KEY,
  published_date TEXT NOT NULL,
  title TEXT NOT NULL,
  pdf_url TEXT NOT NULL,
  pooled_hash TEXT NOT NULL
);
