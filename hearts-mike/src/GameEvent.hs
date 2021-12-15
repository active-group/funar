module GameEvent where

import Cards

{-
data GameEvent =
    TwoOfClubsPlayed Player 
  | PlayersArrived [Player]
  | CardsShuffled [Card] -- gemischte Karten
  | CardsDealt PlayerHands 
  -- | CardsCycled 
  | CardPlayed Card Player
  | TrickTaken Trick Player
  | PlayerTurnChanged Player 
  | PointsAwarded Integer Player 
  | GameEnded Player 



data GameCommand =
    ShuffleCards [Card] -- vor dem Mischen
  | PlayerSitDown Player 
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

