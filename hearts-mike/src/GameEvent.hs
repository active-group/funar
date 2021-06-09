module GameEvent where

import Cards

data Event = 
    CardPlayed Player Card 
  | PlayerTookCards Player [Card]
  | RoundOver       Player [Card] -- redundant zu PlayerTookCards
  | CardsDealt [(Hand, Player)]
-- Alternative:
--  | CardsDealt Hand Player
  | IllegalCardPlayed Player Card
  | GameEnded Player Int
  | CardsShuffled [Card]
  
-- data Command =