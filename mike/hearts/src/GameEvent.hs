module GameEvent where

import Cards

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Repräsentation eines Ereignisses (das passiert ist)

-- Event-Sourcing: Events bilden bestandsführende System

-- - in der Vergangenheit
-- - fachlich, nicht technisch (Fakt)
-- - müssen ganze Geschichte, enthalten alle bekannte Information
-- - Redunanz
-- - sollten nicht resultierenden Zustand enthalten
{-
data GameEvent =
    CardsDealt [(Player, Set Card)]
  | TwoOfClubsPlayed Player
  | CardPlayed Player Card
  | TurnChanged Player
  | TrickTaken Player (Set Card)
  | CardsPassed Player (Set Card)
  | GameWon Player
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

-- Command: Wunsch, daß etwas passieren möge
-- - in der Zukunft
-- - passiert nicht unbedingt
data GameCommand =
    PlayCard Player Card
  | DealHands (Map Player Hand)
  deriving (Show, Eq)
