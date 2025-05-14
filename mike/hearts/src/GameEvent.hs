module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event:
-- Ein Objekt, das ein Ereignis repräsentiert, das in der Vergangenheit passiert ist.

-- in der Vergangenheit (-> Vergangenheitsform bei der Benamsung)
-- Muster: Nomen + Verb
-- nicht veränderbar ,,, Fakt
-- fachlich (vs. technisch)
-- kein Zustand
-- Redundanz OK

-- -> Event-Sourcing
-- in den Event muß alles drinstehen ... Geschichte der Domäne

-- vs. Commands: Repräsentation eines Wusches, daß etwas in der Zukunft passieren möge

{-
data GameEvent =
    GameStarted Player Player Player Player
  | DeckShuffled [Card] -- gemischten Karten
  | CardsDistributed (Map Player Hand)
  | CardPlayed Card Player
  | RoundPlayed Player Trick -- Integer
  | NextPlayerChanged Player
  | GameEnded Player --- Gewinner

data GameCommand =
    RegisterPlayer Player -- ... oder ...
  | StartGame Player Player Player Player
  | PlayCard Card Player
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
