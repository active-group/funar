module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event:
-- Datenobjekt, das ein Ereignis repräsentiert, das passiert ist.

-- - in der Vergangenheit
-- - fachlich motiviert
-- - sollten i.d.R. nicht Zustand enthalten

-- Event-Sourcing:
-- - die Events enthalten alle Infos über Ereignis
-- - ... sollten vollständig sein
-- - Redundanz OK

-- Command:
-- Datenobjekt, das einen Wunsch repräsentiert,
-- daß etwas in der Zukunft passiert

{-
data GameEvent =
    GameStarted [Player]
  | CardsDealt (Map Player Hand)
  | CardPlayed Player Card
  | RoundLost Player Trick  -- wer mußte den Stich nehmen
  | GameWon Player
  | PlayerTurnChanged Player
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | InvalidCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

data GameCommand =
    DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)
