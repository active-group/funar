{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Repräsentation eines Ereignisses

-- - in Vergangenheit passiert
-- - fachlich: Geschichte unserer Domäne
-- - alles, was man weiß
-- - Redundanz OK
-- - nicht "aktuellen Zustand"

-- Command: Repräsentation eines Wunsches
-- - noch nicht passiert
-- - in Zukunft
-- - passiert vielleicht gar nicht
{-
newtype Score = Score Int

data GameEvent =
    GameStarted Player Player Player Player
  | CardsDealt (Hand, Player) (Hand, Player) 
               (Hand, Player) (Hand, Player)
  | RoundPlayed Player Trick
  | CardPlayed Player Card
  | GameEnded (Player, Score) (Player, Score)
              (Player, Score) (Player, Score)
  | MatchEnded Player

data GameCommand =
    PlayCard Player Card
  | PickUpTrick Player Trick
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

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | IsLegalMove Player Card (Bool -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isLegalMoveM :: Player -> Card -> Game Bool
isLegalMoveM player card = IsLegalMove player card Return

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() -> (callback ()) >>= next)
    (>>=) (IsLegalMove player card callback) next =
        IsLegalMove player card 
          (\isLegal -> callback isLegal >>= next)
    (>>=) (Return result) next = next result

    return :: a -> Game a
    return = Return

-- data Maybe a = Nothing | Just a

-- Maybe Player: ist das Spiel vorbei und wenn ja, wer hat gewonnen
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
    let events = map (uncurry HandDealt) (Map.toList hands)
        commands = map recordEventM events
        -- brauchen [Game ()] -> Game ()
    in do sequence_ commands
          return Nothing
tableProcessCommandM (PlayCard player card) =
    do isLegal <- isLegalMoveM player card
       if isLegal 
       then do -- your code goes here
               return undefined
       else do recordEventM (IllegalCardAttempted player card)
               return Nothing