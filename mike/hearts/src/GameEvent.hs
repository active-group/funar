module GameEvent where

import Cards

-- Event:
-- Repräsentation eines Ereignisses (als DATEN), das passiert ist.
-- - in der Vergangenheit
-- - fachlich (kein INSERT-Statement)
-- - sollten nicht den sich daraus ergebenden Zustand enthalten

-- Event-Sourcing (Events sind der bestandführende "Speicher"):
-- - vollständig
-- - Redundanz OK
-- - "lieber zu viel als zu wenig"

-- vs. Commands

data GameEvent =
