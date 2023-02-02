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

data GameEvent
    = GameStarted GameId     -- Ein Spiel besteht aus mehreren Runden
    | RoundStarted -- Jede Runde besteht aus 13 Stichen/Tricks
    | HandsDistributed (Map Player Hand)
    | StartingPlayerSelected Player
    | CardPlayed Player Card
    | TrickCompleted
    | TrickTaken Player Trick
    | NextPlayerSelected Player
    | RoundCompleted [Player] -- Gewinner der Runde
    | GameCompleted [Player]
    | PlayerResigned Player
    | IllegalCardAttempted Player Card
    deriving (Eq, Show)

data GameCommand
    = PlayCard Player Card
    | DealHands PlayerHands