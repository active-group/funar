module GameEvent where

import Cards

data GameEvent =
    HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardPlayed Player Card
  | GameEnded Player
  deriving Show

data GameCommand =
    DealHands PlayerHands
  | PlayCard Player Card
  deriving Show

{-
-- Event: etwas, das in der Vergangenheit passiert ist
-- Die Events beschreiben, alles, was passiert ist.
-- Sie sollten aus der Domäne kommen.
-- Sie dürfen redundant sein.
data GameEvent =
      PlayerArrived Player
    | GameStarted [Player]

    | CardsDealt PlayerHands
    | CardPlayed Player Card

    | TrickDone Trick Player
    | PlayerTurnChanged Player
    | GameOver Player -- Gewinner
    | InvalidMove Player Card
 

-- Command: Beschreibung von etwas, das passieren soll.
-- != Events
data GameCommand =
      EnterGame Player
    | StartGame PlayerHands
    | PlayCard Player Card
-}