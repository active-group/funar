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

{-
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
data GameCommand =
    ChooseCard Player Card

-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

data GameCommand
  = DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)

{-
data DealHandsCommand = DealHands (Map Player Hand)

data PlayCardCommand = PlayCard Player Card

executeDealHandsCommand (DealHands map) = ...

executeDealHandsCommand (DealHands map)
-}