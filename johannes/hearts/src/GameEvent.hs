{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Events: Domain-Event
-- - Log anlegen von Dingen, die in der Anwendung/Domäne passieren
-- - Log sollte alles enthalten, was passiert ist -> Erzählung
-- - gehen davon aus, dass sie in der Reihenfolge des Passierens aufgezeichnet werden

-- Event:
-- - in der Vergangenheit passiert!
-- - fachlich motiviert
-- - Redundanz ist OK
-- - (keinen Zustand reinschreiben)
-- - Events sind (einzige) Quelle der Wahrheit im System

-- Command:
-- - Bitte, dass etwas passieren soll (in der Zukunft)

-- Event-Storming

type GameId = String

-- data GameEvent
--     = GameStarted GameId     -- Ein Spiel besteht aus mehreren Runden
--     | RoundStarted -- Jede Runde besteht aus 13 Stichen/Tricks
--     | HandsDistributed (Map Player Hand)
--     | StartingPlayerSelected Player
--     | CardPlayed Player Card
--     | TrickCompleted
--     | TrickTaken Player Trick
--     | NextPlayerSelected Player
--     | RoundCompleted [Player] -- Gewinner der Runde
--     | GameCompleted [Player]
--     | PlayerResigned Player
--     | IllegalCardAttempted Player Card
--     deriving (Eq, Show)

-- data GameCommand
--     = PlayCard Player Card
--     | DealHands (Map Player Hand)
--     deriving (Eq, Show)

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Eq, Show)

data GameCommand
    = PlayCard Player Card
    | DealHands (Map Player Hand)
    deriving (Eq, Show)




-- Spielablauf modellieren

-- Spielablauf als Daten (wie DB)
data Game a
    = IsCardValid Player Card (Bool -> Game a)
    | RecordEvent GameEvent (() -> Game a)
    | TurnOverTrick (Maybe (Trick, Player) -> Game a)
    | PlayerAfter Player (Player -> Game a)
    | IsGameOver (Maybe Player -> Game a)
    | GetNextCommand (GameCommand -> Game a)
    | Return a

playerAfter :: Player -> Game Player
playerAfter player = PlayerAfter player Return

isCardValid :: Player -> Card -> Game Bool
isCardValid player card = IsCardValid player card Return

recordEvent :: GameEvent -> Game ()
recordEvent event = RecordEvent event Return

turnOverTrickM :: Game (Maybe (Trick, Player))
turnOverTrickM = TurnOverTrick Return

isGameOverM :: Game (Maybe Player)
isGameOverM = IsGameOver Return

getNextCommand :: Game GameCommand
getNextCommand = GetNextCommand Return

instance Functor Game where
instance Applicative Game where

instance Monad Game where
    return :: a -> Game a
    return = Return

    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (Return value) next = next value
    (>>=) (IsCardValid player card callback) next =
        IsCardValid player card (\ value -> (>>=) (callback value) next)
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\ value -> (>>=) (callback value) next)
    (>>=) (TurnOverTrick callback) next =
        TurnOverTrick (\ value -> (>>=) (callback value) next)
    (>>=) (PlayerAfter player callback) next =
        PlayerAfter player (\ value -> (>>=) (callback value) next)
    (>>=) (IsGameOver callback) next =
        IsGameOver (\ value -> (>>=) (callback value) next)

-- _ein_ Command abarbeiten
tableProcessCommand :: GameCommand -> Game (Maybe Player)
tableProcessCommand (DealHands playerHands) = undefined
tableProcessCommand (PlayCard player card) = do
    -- Daniel: müssen wir nicht erst prüfen, ob Karten ausgeteilt wurden?
    canPlay <- isCardValid player card
    if canPlay
        then do
            recordEvent (LegalCardPlayed player card)
            turnOverTrick <- turnOverTrickM
            case turnOverTrick of
                Nothing -> do
                    nextPlayer <- playerAfter player
                    recordEvent (PlayerTurnChanged nextPlayer)
                    return Nothing
                Just (trick, trickTaker) -> do
                    recordEvent (TrickTaken trickTaker trick)
                    maybeWinner <- isGameOverM
                    case maybeWinner of
                        Nothing -> do
                            recordEvent (PlayerTurnChanged trickTaker)
                            return Nothing
                        Just winner -> do
                            recordEvent (GameEnded winner)
                            return (Just winner)
        else do
            recordEvent (IllegalCardAttempted player card)
            return Nothing

-- Gesamtes Spiel ablaufen lassen
tableLoopM :: GameCommand -> Game Player
tableLoopM command = do
    maybeWinner <- tableProcessCommand command
    case maybeWinner of
        Nothing -> do
            nextCommand <- getNextCommand
            tableLoopM nextCommand
        Just winner ->
            return winner