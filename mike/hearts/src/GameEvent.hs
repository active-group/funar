{-# LANGUAGE InstanceSigs #-}
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

{-
data GameEvent =
    CardPlayed Player Card
  | CardDealt Player Card
  -- ab hier ist alles andere unnötig:
  | CardConsidered Player Card
  | CardRejected Player Card
  | TrickTaken Player [Card]
  | RoundOver
  | PlayerSelected Player  
  | GameStarted
  | GameEnded (Map Player Integer)

data GameCommand =
    StartGame [Player]
  | PlayCard Player Card

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

-- Monade für den Spielablauf
data Game a =
    Return a

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return :: a -> Game a
    return = Return

-- tableProcessCommand :: GameCommand -> TableState -> [GameEvent]
-- ist das Spiel vorbei - und wer hat gewonnen?
-- dabei sollen Events protokolliert werden
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) = undefined
tableProcessCommandM (PlayCard player card) = undefined

