module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{- Event:
- Beschreibung (als Daten!) von einem Ereignis in der Vergangenheit
- fachlich
- enthalten nicht den neuen Zustand
Event-Sourcing:
- Events erzählen die gesamte Geschichte der Domäne
- (was passiert? von wem? wann?)
- Redundanz ist OK

vs.
Command:
- Wunsch, daß etwas passieren möge (in der Zukunft)
- könnte nicht passieren
-}

{-
data GameEvent =
    CardLaid Player Card -- Zeitpunkt
  | TrickTaken Player Trick
  | CardsDealt (Map Player Hand)
  | GameStarted [Player]
  | PlayerWon Player
  | PlayerLost Player
  | GameEnded (Map Player Integer)
  | IllegalCardAttempted Player Card

data GameCommand =
    PlayCard Player Card
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
