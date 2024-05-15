module GameEvent where

import Cards

import qualified Data.Map as Map
import Data.Map (Map)

-- Event: Repräsentation eines Ereignisses, das passiert ist

-- in der Vergangenheit passiert
-- steht alles drin, was wir wissen / was relevant ist
-- Redundanz OK
-- fachlich getrieben

data GameEvent =
    CardsShuffled [Card]
  | CardsDealt Hand Hand Hand Hand
  | CardPlayed Card Player
  | TrickTaken Trick Player
  | GameWon Player Integer

-- Command: Wunsch, daß etwas passieren soll
data GameCommand =
    PlayCard Card Player


-- Antipattern: Events und Commands sind das gleiche
-- Antipattern:

-- eigentlich:
-- f :: Input1 -> Input2 -> Input3 -> Output
-- f input1 input2 input3 = output

-- ->

-- data fInputCommand { input1 :: Input1, input2 :: Input2, input3 :: Input3}
-- f command = ... input1 command ... input2 command ... input3 command
