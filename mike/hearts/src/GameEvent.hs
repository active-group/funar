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
  | PlayValid Player Card (Bool -> Game a)
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | NextPlayer Player (Player -> Game a)
  | IsGameOver (Maybe Player -> Game a)  
  | Done a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Done

playValidM :: Player -> Card -> Game Bool
playValidM player card = PlayValid player card Done

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Done
 
nextPlayerM :: Player -> Game Player
nextPlayerM player = NextPlayer player Done

isGameOverM :: Game (Maybe Player)
isGameOverM = IsGameOver Done

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return = Done
    (>>=) (Done result) next = next result
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() -> (callback () >>= next))
    (>>=) (PlayValid player card callback) next =
        PlayValid player card (\isValid -> callback isValid >>= next)
    (>>=) (TurnOverTrick callback) next =
        TurnOverTrick (\over -> callback over >>= next)
    (>>=) (NextPlayer player callback) next =
        NextPlayer player (\player' -> callback player' >>= next)
    (>>=) (IsGameOver callback) next =
        IsGameOver (\winner -> callback winner >>= next)

-- Rückgabe: Gewinner:in, falls Spiel vorbei
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
    let events = map (\(player, hand) -> HandDealt player hand) 
                     (Map.toList hands)
    in do mapM_ recordEventM events
          return Nothing

tableProcessCommandM (PlayCard player card) =
    do isValid <- playValidM player card
       if isValid
       then do recordEventM (LegalCardPlayed player card)
               over <- turnOverTrickM
               case over of
                  Nothing ->
                    do nextPlayer <- nextPlayerM player
                       recordEventM (PlayerTurnChanged nextPlayer)
                       return Nothing
                  Just (trick, trickTaker) ->
                    do recordEventM (TrickTaken trickTaker trick)
                       winner <- isGameOverM
                       case winner of
                        Nothing ->
                            do recordEventM (PlayerTurnChanged trickTaker)
                               return Nothing
                        Just winner ->
                            do recordEventM (GameEnded winner)
                               return (Just winner)
       else do recordEventM (IllegalCardAttempted player card)
               return Nothing
