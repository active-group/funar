module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Objekt, das ein fachliches Ereignis in der Domäne repräsentiert.

-- - in der Vergangenheit passiert
-- - können gesamte Geschichte der Domäne erzählen
-- - Redundanz ist OK

-- Command: Objekt, das einen (fachlichen) Wunsch repräsentiert.

-- - in der Zukunft
-- - != Event
{-
type RandomSeed = Integer

data GameEvent =
    GameStarted [Player]
  | FirstMoveMade Player
  | DeckShuffled RandomSeed
  | CardsDistributed Hand Hand Hand Hand
  | CardPlayed Player Card
  | RoundFinished Player Trick
  | PilesFinished Pile Pile Pile Pile
  | GameFinished Player
  | TurnChanged Player 

data GameCommand =
    PlayCard Player Card
  | MakeFirstMove Player
  | StartGame [Card] -- Hand Hand Hand Hand
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

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | Done a

instance Monad Game where

recordEventM event = RecordEvent event Done

tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) = 

tableProcessCommandM (PlayCard player card) = undefined