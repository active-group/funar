module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{-
Event: Datenwert, der ein Ereignis beschreibt
- ist passiert - in der Vergangenheit
- alle Informationen über das Ereignis
- Redundanz OK
- Zustand nicht OK
- fachlich, nicht technisch

Commands: Datenwert, der einen Wunsch beschreibt, daß etwas passiert
- in der Zukunft, noch nicht passiert
-}
{-
data GameEvent =
    PlayerTurnChanged Player
  | CardPlayed Player Card
  | StartingCardPlayed Player
  | PlayerWon Player
--  | CardsShuffled [Card] -- auch OK
  | CardsShuffled Hand Hand Hand Hand
  | RoundCompleted Trick Player
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

data Game a =
    Return a


-- Maybe Player: Ist das Spiel und wer hat gewonnen?
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM = undefined