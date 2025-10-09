{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Repräsentation eines Ereignisses (das passiert ist)

-- Event-Sourcing: Events bilden bestandsführende System

-- - in der Vergangenheit
-- - fachlich, nicht technisch (Fakt)
-- - müssen ganze Geschichte, enthalten alle bekannte Information
-- - Redunanz
-- - sollten nicht resultierenden Zustand enthalten
{-
data GameEvent =
    CardsDealt [(Player, Set Card)]
  | TwoOfClubsPlayed Player
  | CardPlayed Player Card
  | TurnChanged Player
  | TrickTaken Player (Set Card)
  | CardsPassed Player (Set Card)
  | GameWon Player
-}

data GameEvent
  = HandDealt Player Hand
  | PlayerTurnChanged Player
  | LegalCardPlayed Player Card
  | TrickTaken Player Trick
  | IllegalCardAttempted Player Card
  | GameEnded Player
  deriving (Show, Eq)

-- Command: Wunsch, daß etwas passieren möge
-- - in der Zukunft
-- - passiert nicht unbedingt
data GameCommand =
    PlayCard Player Card
  | DealHands (Map Player Hand)
  deriving (Show, Eq)

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

instance Functor Game where
instance Applicative Game where


instance Monad Game where
    return :: a -> Game a
    return = Return
    
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (Return result) next = next result
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() ->
            callback () >>= next)

-- Just winner, wenn das Spiel vorbei
-- muß Events generieren
tableProcessCommand :: GameCommand -> Game (Maybe Player)
tableProcessCommand (DealHands hands) =
    let pairs = Map.toList hands
        events = map (uncurry HandDealt) pairs
    in do mapM_ recordEventM events
          return Nothing -- Spiel noch nicht vorbei
tableProcessCommand (PlayCard player card) = undefined