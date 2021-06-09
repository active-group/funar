module GameEvent where

import Cards

{-
data Event = 
    CardsShuffled [Card]
  | CardPlayed Player Card 
  -- alles andere redundant:
  | PlayerTookCards Player [Card]
  | RoundOver       Player [Card] -- redundant zu PlayerTookCards
  | CardsDealt [(Hand, Player)]
-- Alternative:
--  | CardsDealt Hand Player
  | IllegalCardPlayed Player Card
  | GameEnded Player Int
  | GameStarted [Player]
  | PlayerTurnChanged Player

data Command =
     ShuffleCards [Card]
   | DealCards [(Player, Card)]
   | PlayCard Player Card

   -}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardPlayed Player Card
  | GameEnded Player
  deriving (Eq, Show)

data GameCommand
  = DealHands PlayerHands
  | PlayCard Player Card
  deriving (Eq, Show)

