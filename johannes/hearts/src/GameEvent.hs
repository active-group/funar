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
    | Return a

isCardValid :: Player -> Card -> Game Bool
isCardValid player card = IsCardValid player card Return

instance Functor Game where
instance Applicative Game where

instance Monad Game where
    return :: a -> Game a
    return = Return

    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (Return value) next = next value
    (>>=) (IsCardValid player card callback) next =
        IsCardValid player card (\ value -> (>>=) (callback value) next)

-- _ein_ Command abarbeiten
tableProcessCommand :: GameCommand -> Game (Maybe Player)
tableProcessCommand (DealHands playerHands) = undefined
tableProcessCommand (PlayCard player card) = do
    -- Daniel: müssen wir nicht erst prüfen, ob Karten ausgeteilt wurden?
    canPlay <- isCardValid player card
    if canPlay
        then undefined
        else undefined