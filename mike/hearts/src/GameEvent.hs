module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Objekt, das ein fachliches Ereignis in der Domäne repräsentiert.

-- - in der Vergangenheit passiert
-- - können gesamte Geschichte der Domäne erzählen
-- - Redundanz ist OK

-- Command: Objekt, das einen (fachlichen) Wunsch repräsentiert.

-- - in der Zukunft
-- - != Event

data GameEvent =
    GameStarted [Player]
  | FirstMoveMade Player
  | CardsDistributed Hand Hand Hand Hand
  | CardPlayed Player Card
  | RoundFinished Player Trick
  | GameFinished Player
  | TurnChanged Player 

-- data GameCommand =
