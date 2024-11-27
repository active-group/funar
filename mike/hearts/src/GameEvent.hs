module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event:
-- - Beschreibung von Ereignis, das passiert ist
-- - Reihenfolge (wahrscheinlich) wichtig
-- - fachlich
-- - (unverändlich)
-- - "vollständig"
-- - Redundanz OK

data GameEvent =
    GameStarted [Player]
  | CardsDealt [(Player, Hand)]
  | PlayerPlayedCard Player Card
  | RoundPlayed Player [Card]
  | GameEnded Player
  | PlayerTurnChanged Player
  deriving Show

-- Command:
-- Beschreibung eines Wunsches, daß etwas passieren möge
-- ... noch nicht in der Vergangenheit