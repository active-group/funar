module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event-Sourcing

-- vs. Data Warehouse: aktuelle Zustand

-- Wir führen ein Log-Buch über das, was in der Domäne passiert ist.

-- 0. Log der Vergangenheit / Fakt
-- 1. fachlich
-- 2. vollständig (u.U. Event-Log einziger Datenbestand)
-- 3. Redundanz OK
-- 4. nicht den "aktuellen" Zustand mitführen
{-
data GameEvent =
    CardPlayed Card Player
  | GameStarted [Player] PlayerHands
  | CardsTaken Player [Card]
  | PointScored Player Integer
  | GameEnded Player Integer (Map Player Integer)
  | PlayerRoundStarted Player
  | PlayerRoundFinished Player
-}
data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Eq, Show)
-- Command:
-- Wunsch, daß etwas in der Zukunft passiert.
-- != Event
-- data GameCommand

data GameCommand
  = DealHands PlayerHands
  | PlayCard Player Card
  deriving (Eq, Show)
