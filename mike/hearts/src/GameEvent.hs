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
