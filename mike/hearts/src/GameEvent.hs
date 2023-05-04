module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Objekt, das ein Ereignis beschreibt, das bereits passiert ist.

-- - in der Vergangenheit
-- - sollte möglichst alle bekannten Informationen enthalten
-- - "die ganze Geschichte der Domäne erzählen"
-- - Redundanz ist OK
-- - (laß den Zustand da raus)
{-
data GameEvent =
    CardPlayed Card Player
  | CardsDistributed [(Player, Hand)]
  | TrickTaken Player Trick
  | GameFinished Player
  | RoundFinished [(Player, Integer)] -- Punkte pro Spieler
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)


-- vs. Command (optional)
-- Objekt, das einen Wunsch beschreibt, dass etwas passieren möge
-- != Event

{-
data GameCommands =
    PlayCard Card Player Integer -- welche Position im Stich
-}

data GameCommand
  = DealHands (Map Player Hand)
  | PlayCard Player Card
  deriving (Show, Eq)
