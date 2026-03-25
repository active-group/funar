{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Ein Event ist eine Repräsentation eines Ereignisses.
-- - ist in der Vergangenheit
-- - fachliche Events
-- - enthalten nicht den neuen Zustand
-- Event-Sourcing:
-- - Events erzählen alles
-- - Redundanz OK
{-
data GameEvent =
    DeckShuffled [Card]
  | CardsDealt [Card] [Card] [Card] [Card]
  | PlayersArrived Player Player Player Player
  | CardPlayed Card Player
  | CardsTaken [Card] Player
  | GameEnd Integer Integer Integer Integer
  | PlayerChanged Player

-- vs. Commands: Repräsentation eines Wunsches, daß etwas passieren möge.
-- - in der Zukunft
-- - "noch kein Fakt"
-- - kein Event
data GameCommand =
    NextPlayer Player
  | PlayCard Card Player
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

-- Spielablauf
data Game a 
    = RecordEvent GameEvent (() -> Game a)
    | IsValid Player Card (Bool -> Game a)
    | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isValidM :: Player -> Card -> Game Bool
isValidM player card = IsValid player card Return

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return :: a -> Game a
    return = Return
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() -> callback () >>= next)
    (>>=) (IsValid player card callback) next =
        IsValid player card (\valid -> callback valid >>= next)
    (>>=) (Return result) next = next result

tableProcessCommandM :: GameCommand -> Game (Maybe Player) -- wenn Spiel vorbei
tableProcessCommandM (DealHands hands) = 
    let pairs = Map.toList hands
        events = fmap (uncurry HandDealt) pairs 
    in do mapM_ recordEventM events
          return Nothing
tableProcessCommandM (PlayCard player card) = 
--    recordEventM (IllegalCardAttempted player card)
--      >>= (\() -> return Nothing)
    do valid <- isValidM player card
       if valid
       then undefined
       else do recordEventM (IllegalCardAttempted player card)
               return Nothing
