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
