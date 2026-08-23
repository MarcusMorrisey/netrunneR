-- NetrunnerDB lineage processed schema: review and ruling tables
-- populated by R/build-nrdb.R from the reviews/rulings endpoints.
-- Column names match the real API response fields exactly (confirmed
-- live against netrunnerdb.com/api/2.0/public) -- rulings carry no id
-- field at all, so the ruling table has no primary key.
CREATE TABLE review (
  id TEXT PRIMARY KEY,
  title TEXT,
  user TEXT,
  ruling TEXT,
  votes INTEGER,
  comments INTEGER,
  date_create TEXT,
  date_update TEXT
);

CREATE TABLE ruling (
  title TEXT,
  ruling TEXT,
  date_update TEXT,
  nsg_rules_team_verified INTEGER
);
