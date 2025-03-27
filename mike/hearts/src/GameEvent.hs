{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{-
Event: Datenwert, der ein Ereignis beschreibt
- ist passiert - in der Vergangenheit
- alle Informationen über das Ereignis
- Redundanz OK
- Zustand nicht OK
- fachlich, nicht technisch

Commands: Datenwert, der einen Wunsch beschreibt, daß etwas passiert
- in der Zukunft, noch nicht passiert
-}
{-
data GameEvent =
    PlayerTurnChanged Player
  | CardPlayed Player Card
  | StartingCardPlayed Player
  | PlayerWon Player
--  | CardsShuffled [Card] -- auch OK
  | CardsShuffled Hand Hand Hand Hand
  | RoundCompleted Trick Player
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

data Game a =
    Return a
  | RecordEvent GameEvent (() -> Game a)

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

instance Functor Game where
    -- later maybe
instance Applicative Game where
    -- later maybe
    pure :: a -> Game a
    pure = Return

instance Monad Game where
    return :: a -> Game a
    return = pure
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (Return result) next = next result

-- Maybe Player: Ist das Spiel und wer hat gewonnen?
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) = undefined
tableProcessCommandM (PlayCard player card) = undefined