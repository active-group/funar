module GameEvent where

import Data.Set (Set)
import Data.Map.Strict (Map)

import Cards

-- Event-Sourcing

-- Wir führen ein explizites Logbuch darüber, was im Spiel passiert.
-- ==> brauchen Datentyp für sogenannte Events
-- Die Events erzählen die gesamte Geschichte der Domäne.

-- Alles muß da rein.
-- Fachlich.
-- Redundanz ist OK.
-- Kein "Zustand".
-- Vergangenheit.

-- Commands
-- Repräsentation eines Wunsches, daß etwas in der Zukunft passiert.

-- != Events

-- alles bis auf CardDealt und CardPlayed ist redundant
{-
data GameEvent =
    CardDealt Card Player
    -- alternativ:
    -- CardsDealt Player Hand
    -- HandsDealt PlayerHands
  | PlayerJoined Player
  | PlayerLeft Player
  | CardsCreated (Set Card)
  | CardPlayed Player Card
  | RoundFinished Player Trick
  | GameOver (Map Player (Set Card)) (Map Player Integer) Player
  | RoundStarted Player

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
  deriving (Eq, Show)

data GameCommand
  = DealHands PlayerHands
  | PlayCard Player Card
  deriving (Eq, Show)
