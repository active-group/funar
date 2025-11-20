{-# LANGUAGE InstanceSigs #-}
module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

{- Event:
- Beschreibung (als Daten!) von einem Ereignis in der Vergangenheit
- fachlich
- enthalten nicht den neuen Zustand
Event-Sourcing:
- Events erzählen die gesamte Geschichte der Domäne
- (was passiert? von wem? wann?)
- Redundanz ist OK

vs.
Command:
- Wunsch, daß etwas passieren möge (in der Zukunft)
- könnte nicht passieren
-}

{-
data GameEvent =
    CardLaid Player Card -- Zeitpunkt
  | TrickTaken Player Trick
  | CardsDealt (Map Player Hand)
  | GameStarted [Player]
  | PlayerWon Player
  | PlayerLost Player
  | GameEnded (Map Player Integer)
  | IllegalCardAttempted Player Card

data GameCommand =
    PlayCard Player Card
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

-- Die Spiel-Monade
data Game a =
    RecordEvent GameEvent (() -> Game a)
  | IsPlayCardValid Player Card (Bool -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

isPlayCardValidM :: Player -> Card -> Game Bool
isPlayCardValidM player card = IsPlayCardValid player card Return

instance Functor Game where

instance Applicative Game where

instance Monad Game where
    return :: a -> Game a
    return = Return
    (>>=) :: Game a -> (a -> Game b) -> Game b
    (>>=) (RecordEvent event callback) next =
        RecordEvent event (\() -> 
            callback () >>= next)
    (>>=) (IsPlayCardValid player card callback) next =
        IsPlayCardValid player card (\valid ->
            callback valid >>= next)
    (>>=) (Return a) next = next a

-- sagt, ob das Spiel vorbei ist und wer gewonnen hat
tableProcessCommandM :: GameCommand -> Game (Maybe Player)
tableProcessCommandM (DealHands hands) =
    let events = map (uncurry HandDealt) (Map.toList hands)
    in do mapM_ recordEventM events
          return Nothing -- Spiel noch nicht vorbei
tableProcessCommandM (PlayCard player card) =
    do valid <- isPlayCardValidM player card
       if valid
       then do recordEventM (LegalCardPlayed player card)
               undefined
       else do recordEventM (IllegalCardAttempted player card)
               return Nothing
