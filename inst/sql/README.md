# inst/sql

## Overview

One DDL file per lineage, applied fresh to each build's SQLite file by
`apply_schema()`. There are no migrations: a build never alters an existing
database, it creates a new one, so the DDL is always the whole truth about a
release's shape.

## Invariants

**A DDL edit is never local to one lineage.** Every file under `schema/` is
hashed into `build_revision()`, which is a single value shared by all six
lineages. Changing `abr.sql` therefore forces a new release of cardpool,
implementation, nrdb and rules as well, none of whose data changed. That is
deliberate -- the revision answers "was this release built by the same code and
the same schema", and a partial answer to that is worse than an over-broad one
-- but it means a one-column addition costs six rebuilds and six promotions.

**A column admitted here must also be admitted upstream.** The build layer
selects with `all_of()` against an allowlist before writing, so a column that
exists in the DDL and not in the allowlist is never populated, and one in the
allowlist but not the DDL fails the write. Both lists have to move together.

**Nullability is a claim about upstream, not a preference.** `NOT NULL` on a
mirrored column asserts that the source always sends it. When that turns out to
be false the whole build fails over one bad record, which is why columns whose
upstream carries occasional junk -- abr's `type`, where one record holds a
dropdown separator rather than a value -- are declared nullable and cleaned in
the view layer instead.
