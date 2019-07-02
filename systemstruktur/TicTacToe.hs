-- {-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TicTacToe where

import Data.List as List

data Position = Position NoCro NoCro NoCro
                         NoCro NoCro NoCro
                         NoCro NoCro NoCro
  deriving Show

position0 = Position Empty Empty Empty Empty Empty Empty Empty Empty Empty

listToPosition [ul, um, ur, ml, mm,  mr, ll, lm, lr] = Position ul um ur ml mm  mr ll lm lr
positionToList (Position ul um ur ml mm  mr ll lm lr) = [ul, um, ur, ml, mm,  mr, ll, lm, lr]

data NoCro = X | O | Empty
  deriving (Eq, Show)

showNoCro X = "X"
showNoCro O = "O"
showNoCro Empty = " "

-- find nocros along winning axes of TTT
axes :: Position -> [[NoCro]]
axes (Position ul um ur ml mm mr ll lm lr) =
  [[ul, um, ur], [ml, mm, mr], [ll, lm, lr], -- lines
   [ul, ml, ll], [um, mm, lm], [ur, mr, lr], -- columns
   [ul, mm, lr], [ur, mm, ll]] -- diagonals
