-- Append-only procurement/acceptance ledger.
--
-- No UPDATE and no DELETE is issued against this table anywhere in the actor:
-- a correction is a new fact, never an edited one. `seq` gives the total order
-- the pure core's MemStore provides via a counter.
--
-- The pairing that matters: an :accepted fact and a :paid fact for the same
-- (order_id, locale). A :paid with no preceding :accepted means money left for
-- work the governor refused — see translation.store/paid-without-acceptance.
CREATE TABLE IF NOT EXISTS translation_ledger (
  seq      INTEGER PRIMARY KEY AUTOINCREMENT,
  order_id TEXT NOT NULL,
  fact     TEXT NOT NULL,   -- ordered | planned | accepted | rejected | paid | flagged
  payload  TEXT NOT NULL,   -- the full fact map as JSON
  at       TEXT NOT NULL DEFAULT (datetime('now'))
);

CREATE INDEX IF NOT EXISTS translation_ledger_order
  ON translation_ledger (order_id, seq);
