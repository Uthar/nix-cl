CREATE TABLE IF NOT EXISTS sha256 (
       id       integer PRIMARY KEY AUTOINCREMENT,
       url      text    UNIQUE,
       hash     text
);

CREATE TABLE IF NOT EXISTS system (
       id    integer PRIMARY KEY AUTOINCREMENT,
       name  text    UNIQUE,
       asd   text
);

CREATE TABLE IF NOT EXISTS dep (
       system_id integer REFERENCES system(id),
       dep_id    integer REFERENCES system(id),
       PRIMARY KEY (system_id, dep_id)
);

CREATE TABLE IF NOT EXISTS src (
       sha256_id    integer REFERENCES sha256(id),
       system_id    integer UNIQUE REFERENCES system(id)
);

DROP VIEW IF EXISTS system_view;
CREATE VIEW IF NOT EXISTS system_view AS
  SELECT
    sys.name,
    sys.asd,
    sha.url,
    sha.hash,
    group_concat((
      SELECT name
      FROM system
      WHERE id = dep.dep_id
    ))
  FROM system sys
  JOIN src ON src.system_id = sys.id
  JOIN sha256 sha ON sha.id = src.sha256_id
  LEFT JOIN dep ON dep.system_id = sys.id
  GROUP BY sys.name;
