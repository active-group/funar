{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GameEvent where

import Cards

import qualified Data.Set as Set  
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

-- Event:
-- Objekt, das ein Ereignis beschreibt - etwas, das passiert ist.

-- - in der Vergangenheit
-- - beziehen auf die Domäne
-- - möglichst alle Informationen über das Ereignis
-- - Redundanz OK

{-
data GameEvent =
    CardPlayed Player Card
  | CardsDealt (Map Player Hand)
  | PlayersArrived (Set Player)
  | TrickHappened Player (Set Card) -- spieler hat Karten aufgenommen
  | GameOver Player -- wer hat gewonnen
  | PlayerTurnChanged Player
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

-- Command:
-- Objekt, das einen Wunsch repräsentiert
-- (passiert vielleicht nicht)
data GameCommand
  = DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)

-- Steht in den Events, wer gewonnen hat?
eventsWinner :: [GameEvent] -> Maybe Player
eventsWinner [] = Nothing
eventsWinner (first : rest) =
  case first of
    GameEnded winner -> Just winner
    _ -> eventsWinner rest

-- Spielregel-Monade
data Game a =
    RecordEvent GameEvent (() -> Game a)
  | Done a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Done

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return = Done
    (>>=) (Done result) next = next result

-- Rückgabe: Gewinner:in, falls Spiel vorbei
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) = undefined
tableProcessCommandM (PlayCard player card) = undefined