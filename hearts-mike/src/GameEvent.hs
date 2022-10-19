module GameEvent where

import Data.Set (Set)

import Cards

-- Event-Sourcing: Events als Quelle der Wahrheit / Persistenzmechanismus

-- - in der Vergangenheit passiert
-- - fachlich orientiert (vs. technisch / "Datenbank-Transkation" :-( )
-- - müssen *alles* erzählen
-- - Redundanz OK
-- - keinen Zustand reinschreiben

{-
type Points = Integer

data GameEvent =
      GameStarted [Player] 
    | CardsDealt PlayerHands -- [(Player, Hand)]
    | GameCancelled Player String -- wer war schuld?
    | MoveMade Player Card
    | TrickTaken Player (Set Card) Points
    | TurnChanged Player
    | RoundStarted Player
    | GameEnded Player
-}
data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Eq, Show)


-- Command
-- - "Bitte", dass etwas passiert in der Zukunft
-- - != Event

data GameCommand
  = DealHands PlayerHands
  | PlayCard Player Card
  deriving (Eq, Show)

-- Steht in den Events, wer gewonnen hat?
eventsWinner :: [GameEvent] -> Maybe Player
eventsWinner [] = Nothing
eventsWinner (first : rest) =
  case first of
    GameEnded winner -> Just winner
    _ -> eventsWinner rest
