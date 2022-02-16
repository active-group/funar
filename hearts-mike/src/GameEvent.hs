module GameEvent where

import Cards

-- Was für Ereignisse passieren in meiner Domäne?
-- ---> Datentyp
-- "Ereignisse, die passiert sind" <- Vergangenheit

-- - Events müssen alles erzählen
-- - Redundanz ist OK
-- - fachlich motivieren

-- Event-Sourcing: Die Abfolge von Event-Objekten
-- enthält alle relevanten Daten der Domäne
-- vs. Data Warehouse

-- Commands
-- "Wunsch, daß etwas in der Zukunft passiert"
-- != Events

{-
data GameEvent =
    CardPlayed Player Card
  | GameStarted [Player]
  | PlayerArrived Player 
  | CardDealt Card Player 
  | GameFinished (Map Player Integer)
  | TrickTaken Player Trick
  | CardsRotated Player Player [Card] -- "from" / "to"
  | PlayerLeft Player 
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardPlayed Player Card
  | GameEnded Player
  deriving (Eq, Show)


{-
data GameCommand =
    PlayCard Player Card 
  | StartGame PlayerHands
-}
data GameCommand
  = DealHands PlayerHands
  | PlayCard Player Card
  deriving (Eq, Show)
