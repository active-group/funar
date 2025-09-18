{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- Event:
-- Repräsentation eines Ereignisses (als DATEN), das passiert ist.
-- - in der Vergangenheit
-- - fachlich (kein INSERT-Statement)
-- - sollten nicht den sich daraus ergebenden Zustand enthalten

-- Event-Sourcing (Events sind der bestandführende "Speicher"):
-- - vollständig
-- - Redundanz OK
-- - "lieber zu viel als zu wenig"

-- vs. Commands:
-- Repräsentation eines Wunsches, dass etwas passieren möge
-- => von den Events trennen

{-
data GameEvent =
    CardPlayed Player Card
  | CardDealt Player Card
  -- ab hier ist alles andere unnötig:
  | CardConsidered Player Card
  | CardRejected Player Card
  | TrickTaken Player [Card]
  | RoundOver
  | PlayerSelected Player  
  | GameStarted
  | GameEnded (Map Player Integer)

data GameCommand =
    StartGame [Player]
  | PlayCard Player Card

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

-- Monade für den Spielablauf
data Game a =
      RecordEvent GameEvent (() -> Game a)
    | PlayAllowed Player Card (Bool -> Game a)
    | RoundOverTrick (Maybe (Trick, Player) -> Game a)
    | PlayerAfter Player (Player -> Game a)
    | GameOver (Maybe Player -> Game a)
    | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

playAllowedM :: Player -> Card -> Game Bool
playAllowedM player card = PlayAllowed player card Return

roundOverTrickM :: Game (Maybe (Trick, Player))
roundOverTrickM = RoundOverTrick Return

gameOverM :: Game (Maybe Player)
gameOverM = GameOver Return

playerAfterM :: Player -> Game Player
playerAfterM player = PlayerAfter player Return

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return :: a -> Game a
    return = Return
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (Return result) next = next result
    (>>=) (RecordEvent event callback) next =
        RecordEvent event
          (\() -> callback () >>= next)
    (>>=) (PlayAllowed player card callback) next =
        PlayAllowed player card
          (\allowed -> callback allowed >>= next)
    (>>=) (RoundOverTrick callback) next =
      RoundOverTrick (\r -> callback r >>= next)
    (>>=) (PlayerAfter player callback) next =
      PlayerAfter player (\player -> callback player >>= next)
    (>>=) (GameOver callback) next =
      GameOver (\r -> callback r >>= next)

-- tableProcessCommand :: GameCommand -> TableState -> [GameEvent]
-- ist das Spiel vorbei - und wer hat gewonnen?
-- dabei sollen Events protokolliert werden
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
    let pairs = Map.toList hands
        events = map (uncurry HandDealt) pairs
        records = map recordEventM events
    in do sequence_ records
          return Nothing -- Spiel noch nicht vorbei

tableProcessCommandM (PlayCard player card) =
    do allowed <- playAllowedM player card
       if allowed
       then do recordEventM (LegalCardPlayed player card)
               roundOver <- roundOverTrickM
               case roundOver of
                Just (trick, trickTaker) ->
                  do recordEventM (TrickTaken trickTaker trick)                     
                     over <- gameOverM
                     case over of
                      Just winner ->
                        do recordEventM (GameEnded winner)
                           return (Just winner)
                      Nothing ->
                        do recordEventM (PlayerTurnChanged trickTaker)
                           return Nothing
                Nothing -> undefined
       else do recordEventM (IllegalCardAttempted player card)
               return Nothing


{-
class ProcessManagement {
    processCommand(GameCommand command, EventListener<GameEvent> listener) {
        switch (command) {
           DealHands ... -> eventListener.fire(new HandDealt(...);
                             eventListener.fire(new HandDealt(...);
        }
    }
}

-}