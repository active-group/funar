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
{-
type RandomSeed = Integer

data GameEvent =
    GameStarted [Player]
  | FirstMoveMade Player
  | DeckShuffled RandomSeed
  | CardsDistributed Hand Hand Hand Hand
  | CardPlayed Player Card
  | RoundFinished Player Trick
  | PilesFinished Pile Pile Pile Pile
  | GameFinished Player
  | TurnChanged Player 

data GameCommand =
    PlayCard Player Card
  | MakeFirstMove Player
  | StartGame [Card] -- Hand Hand Hand Hand
-}

