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
    = GameStarted GameId
    | RoundStarted
    | HandsDistributed (Map Player Hand)
    | StartingPlayerSelected Player
    | CardPlayed Player Card
    | TrickCompleted
    | TrickTaken Player Trick
    deriving (Eq, Show)