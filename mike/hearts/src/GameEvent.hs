{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event:
-- - Beschreibung von Ereignis, das passiert ist
-- - Reihenfolge (wahrscheinlich) wichtig
-- - fachlich
-- - (unverändlich)
-- - "vollständig"
-- - Redundanz OK

{-
data GameEvent =
    GameStarted [Player]
  | CardsDealt [(Player, Hand)]
  | PlayerPlayedCard Player Card
  | RoundPlayed Player [Card]
  | GameEnded Player
  | PlayerTurnChanged Player
  deriving Show

-- Command:
-- Beschreibung eines Wunsches, daß etwas passieren möge
-- ... noch nicht in der Vergangenheit
data GameCommand =
    ChooseCard Player Card

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

{-
data DealHandsCommand = DealHands (Map Player Hand)

data PlayCardCommand = PlayCard Player Card

executeDealHandsCommand (DealHands map) = ...

executeDealHandsCommand (DealHands map)
-}

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | IsValidCard Player Card (Bool -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isValidCardM :: Player -> Card -> Game Bool
isValidCardM player card = IsValidCard player card Return

instance Functor Game where

instance Applicative Game where
    pure :: a -> Game a
    pure = Return

instance Monad Game where
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() -> callback () >>= next)
    (>>=) (Return result) next = next result

-- *ein* Command, Ergebnis: Gewinner, falls Spiel zu Ende
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hand) =
    -- HandDealt-Events verzeichnen
    do let pairs = Map.toList hand
       let events = map (uncurry HandDealt) pairs
       -- M für Monade, _ für "kein Ergebnis"
       mapM_ recordEventM events
       return Nothing -- Spiel noch nicht zu Ende
tableProcessCommandM (PlayCard player card) =
    do undefined