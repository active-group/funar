module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Datenrepräsentation von einem Ereignis

-- - ist passiert, in der Vergangenheit
-- - fachlich
-- - vollständig: gesamte Geschichte der Domäne
-- - Redundanz OK
-- - *nicht* Zustand mit ablegen
{-
data GameEvent =
    CardsDealt (Map Player Hand)
  | GameStarted Player Player Player Player
  | CardLaidAtTrickStart Player Card
  | CardLaidIntoTrick Player Card
  | CardsTaken Player [Card]
  | GameWon Player
  deriving Show

-- Command: Repräsentation eines Wunschs, daß etwas passieren soll - in der Zukunft
-- != Event
data GameCommand =
    DealCards Player [Card]
  | StartGame
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



{-
playCard :: Player -> Card -> ... -> ...

--> 

data PlayCardCommand = MkPlayCardCommand Player Card

playCard :: PlayCardCommand -> ... -> ...

playCard p c ... -> playCard (MkPlayerCommand p c) ...

-}

-- als nächstes: Ablauf des Spiels / Anordnung der Events
-- sequentielle Abläufe: Monade

-- Wie werden aus Commands Events?

data Game a =
    RecordEvent GameEvent (() -> Game a)
  | Return a

recordEventM :: GameEvent -> Game ()
recordEventM event = RecordEvent event Return

instance Functor Game

instance Applicative Game

instance Monad Game where
  (>>=) (Return result) next = next result
  return = Return

-- data Maybe a = Nothing | Just a

tableProcessCommandM :: GameCommand -> Game (Maybe Player) -- sagt uns, ob das Spiel vorbei ist und wer gewonnen hat
tableProcessCommandM (DealHands hands) =
  let list = Map.toList hands
      events = map (uncurry HandDealt) list
      games = map recordEventM events
  in do sequence_ games
        return Nothing

tableProcessCommandM (PlayCard player card) = undefined