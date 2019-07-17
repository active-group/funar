-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TicTacToe where

import Data.List as List
import Control.Parallel.Strategies as Par

data Position = Position NoCro NoCro NoCro
                         NoCro NoCro NoCro
                         NoCro NoCro NoCro
  deriving (Show, Eq)

showPosition :: Position -> String
showPosition (Position ul um ur ml mm  mr ll lm lr) =
  (showNoCro ul) ++ " | " ++ (showNoCro um) ++ " | " ++ (showNoCro ur) ++
  "\n----------\n" ++
  (showNoCro ml) ++ " | " ++ (showNoCro mm) ++ " | " ++ (showNoCro mr) ++
  "\n----------\n" ++
  (showNoCro ll) ++ " | " ++ (showNoCro lm) ++ " | " ++ (showNoCro lr)

position0 = Position Empty Empty Empty Empty Empty Empty Empty Empty Empty

listToPosition [ul, um, ur, ml, mm,  mr, ll, lm, lr] = Position ul um ur ml mm  mr ll lm lr
positionToList (Position ul um ur ml mm  mr ll lm lr) = [ul, um, ur, ml, mm,  mr, ll, lm, lr]

data NoCro = X | O | Empty
  deriving (Eq, Show)

showNoCro X = "X"
showNoCro O = "O"
showNoCro Empty = " "

-- positions that can be reached via a single move
moves :: Position -> [Position]
moves position =
  case gameOver position of
    Just _ -> []
    Nothing ->
      map listToPosition (moves' (nextPlayer position) (positionToList position))

-- generate a list of all possible nought / cross placements
moves' :: NoCro -> [NoCro] -> [[NoCro]]
moves' player [] = []
moves' player (Empty : rest) = (player : rest) : (map ((:) Empty) (moves' player rest))
moves' player (p : rest) = map ((:) p) (moves' player rest)

-- find nocros along winning axes of TTT
axes :: Position -> [[NoCro]]
axes (Position ul um ur ml mm mr ll lm lr) =
  [[ul, um, ur], [ml, mm, mr], [ll, lm, lr], -- lines
   [ul, ml, ll], [um, mm, lm], [ur, mr, lr], -- columns
   [ul, mm, lr], [ur, mm, ll]] -- diagonals

-- did Noughts or Crosses win the game already?
won :: Position -> NoCro -> Bool
won position nocro =
  any (all ((==) nocro)) (axes position)

gameOver :: Position -> Maybe NoCro
gameOver position =
  if won position X
  then Just X
  else if won position O
  then Just O
  else if all ((/=) Empty) (positionToList position)
  then Just Empty
  else Nothing

nextPlayer :: Position -> NoCro
nextPlayer position =
  let lis = positionToList position
      xs = filter ((==) X) lis
      os = filter ((==) O) lis
  in if length xs <= length os
     then X
     else O

