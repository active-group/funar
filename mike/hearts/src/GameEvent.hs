module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Ein Event ist eine Repräsentation eines Ereignisses.
-- - ist in der Vergangenheit
-- - fachliche Events
-- - enthalten nicht den neuen Zustand
-- Event-Sourcing:
-- - Events erzählen alles
-- - Redundanz OK
{-
data GameEvent =
    DeckShuffled [Card]
  | CardsDealt [Card] [Card] [Card] [Card]
  | PlayersArrived Player Player Player Player
  | CardPlayed Card Player
  | CardsTaken [Card] Player
  | GameEnd Integer Integer Integer Integer
  | PlayerChanged Player

-- vs. Commands: Repräsentation eines Wunsches, daß etwas passieren möge.
-- - in der Zukunft
-- - "noch kein Fakt"
-- - kein Event
data GameCommand =
    NextPlayer Player
  | PlayCard Card Player
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
