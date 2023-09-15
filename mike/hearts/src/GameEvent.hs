{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event ist eine Repräsentation eines Ereignisses in unserer Domäne.

-- - Event ist in der Vergangenheit passiert.
-- - Event bezieht sich auf die Domäne, nicht die Technik.
-- - Events erzählen die vollständige Geschichte der Domäne.
-- - Redundanz ist OK.

{-
type Points = Integer

data GameEvent = 
    DealHands Player Hand
  | PlayCard Player Card
  | PlayerStartsGame Player
  | AllPlayersHavePlayed Trick
  | PlayerTookTrick Player Trick Points
  | AllCardsPlayed (Map Player Points)
  | TurnChanged Player
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

-- Commands
-- Repräsentation eines Wunsches, dass etwas in der Zukunft passiert.
data GameCommand =
    PlayCard Player Card
  | DealHands (Map Player Hand)
  deriving (Show, Eq)

-- Steht in den Events, wer gewonnen hat?
eventsWinner :: [GameEvent] -> Maybe Player
eventsWinner [] = Nothing
eventsWinner (first : rest) =
  case first of
    GameEnded winner -> Just winner
    _ -> eventsWinner rest

{-
class Monad m where
  return :: a -> m a
  -- "bind" bzw. "flatMap"
  (>>=) :: m a -> (a -> m b) -> m b
-}

-- Spiel(regel)-Monade
data Game a =
    Done a -- gibt es immer
  | RecordEvent GameEvent (() -> Game a)
  | PlayValid Player Card (Bool -> Game a)
  -- wenn die Runde vorbei ist, bekommen wir Stich
  -- und Spieler:in, die ihn aufnehmen musste
  | TurnOverTrick (Maybe (Trick, Player) -> Game a)
  | PlayerAfter Player (Player -> Game a)
  | GameOver (Maybe Player -> Game a)
  | GetCommand (GameCommand -> Game a)

recordEventM :: GameEvent -> Game ()
recordEventM event =
  RecordEvent event Done

playValidM :: Player -> Card -> Game Bool
playValidM player card =
  PlayValid player card Done

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Done

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Done

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Done

instance Functor Game where

instance Applicative Game where

instance Monad Game where
  return :: a -> Game a
  return = Done
  (>>=) :: Game a -> (a -> Game b) -> Game b
  (>>=) (Done result) next = next result
  (>>=) (RecordEvent event callback) next =
    RecordEvent event     (\() ->      callback ()      >>= next)
  (>>=) (PlayValid player card callback) next =
    PlayValid player card (\isValid -> callback isValid >>= next)
  (>>=) (TurnOverTrick callback) next =
    TurnOverTrick (\maybeTrick ->     callback maybeTrick >>= next)
  (>>=) (PlayerAfter player cont) next =
    PlayerAfter player (\player -> cont player >>= next)
  (>>=) (GameOver cont) next =
    GameOver (\won -> cont won >>= next)
  (>>=) (GetCommand callback) next =
    GetCommand (\command -> callback command >>= next)

-- data Maybe a = Just a | Nothing

-- Spielregeln / "Tisch"
-- Commands rein, Events raus, Spiel vorbei?
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) = 
  -- HandDeal-Events
  do mapM_ (\(player, hand) -> recordEventM (HandDealt player hand)) 
           (Map.toList hands)
     return Nothing
  
tableProcessCommandM (PlayCard player card) =
  do isValid <- playValidM player card
     if isValid
     then do recordEventM (LegalCardPlayed player card)
             turnOverTrick <- turnOverTrickM
             case turnOverTrick of
              Nothing -> 
                do 
                  nextPlayer <- playerAfterM player
                  recordEventM (PlayerTurnChanged nextPlayer)
                  return Nothing
              Just (trick, trickTaker) ->
                do
                  recordEventM (TrickTaken trickTaker trick)
                  over <- gameOverM
                  case over of
                    Just winner ->
                      do
                        recordEventM (GameEnded winner)
                        return (Just winner)
                    Nothing ->
                      do
                        recordEventM (PlayerTurnChanged trickTaker)
                        return Nothing
              
     else do recordEventM (IllegalCardAttempted player card)
             return Nothing

-- Gesamte Spiel spielen, ab dem ersten Command
tableLoopM :: GameCommand -> Game Player
tableLoopM command =
  do maybeWinner <- tableProcessCommandM command
     case maybeWinner of
      Just winner -> return winner
      Nothing -> GetCommand tableCommand
--                 do command <- GetCommand Done
--                    tableLoopM command