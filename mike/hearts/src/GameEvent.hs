module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{-
Event: Objekt, das ein Ereignis beschreibt

- ... Ereignis in der Domäne
- ... ist (in der Vergangenheit) passiert
- ... sollten alles beschreiben, was passiert
- ... Redundanz ist OK

Command: Objekt, das einen Wunsch beschreibt
- ... ist noch nicht passiert (=> in der Zukunft)

-}
{-
data GameEvent =
    CardsShuffled [Card]
  | CardsDealt Player (Map Player Hand)
  | CardPlayed Player Card
  | TrickDone Player Trick
  | GameEnded Player
  deriving Show

data GameCommand =
      Shuffle [Card]
    | PlayCard Player Card
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

-- Steht in den Events, wer gewonnen hat?
eventsWinner :: [GameEvent] -> Maybe Player
eventsWinner [] = Nothing
eventsWinner (first : rest) =
  case first of
    GameEnded winner -> Just winner
    _ -> eventsWinner rest

-- Monade für den Spielablauf
data Game a =
    