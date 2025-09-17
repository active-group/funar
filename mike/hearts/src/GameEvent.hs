module GameEvent where

import Cards

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- Event:
-- Repräsentation eines Ereignisses (als DATEN), das passiert ist.
-- - in der Vergangenheit
-- - fachlich (kein INSERT-Statement)
-- - sollten nicht den sich daraus ergebenden Zustand enthalten

-- Event-Sourcing (Events sind der bestandführende "Speicher"):
-- - vollständig
-- - Redundanz OK
-- - "lieber zu viel als zu wenig"

-- vs. Commands:
-- Repräsentation eines Wunsches, dass etwas passieren möge
-- => von den Events trennen

data GameEvent =
    CardPlayed Player Card
  | CardDealt Player Card
  | CardConsidered Player Card
  | CardRejected Player Card
  | TrickTaken Player [Card]
  | RoundOver
  | PlayerSelected Player  
  | GameEnded (Map Player Integer)