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

showPosition :: Position -> String
showPosition (Position ul um ur ml mm  mr ll lm lr) =
  (showNoCro ul) ++ " | " ++ (showNoCro um) ++ " | " ++ (showNoCro ur) ++
  "\n----------\n" ++
  (showNoCro ml) ++ " | " ++ (showNoCro mm) ++ " | " ++ (showNoCro mr) ++
  "\n----------\n" ++
  (showNoCro ll) ++ " | " ++ (showNoCro lm) ++ " | " ++ (showNoCro lr)

data Player = PX | PO

-- find nocros along winning axes of TTT
axes :: Position -> [[NoCro]]
axes (Position ul um ur ml mm mr ll lm lr) =
  [[ul, um, ur], [ml, mm, mr], [ll, lm, lr], -- lines
   [ul, ml, ll], [um, mm, lm], [ur, mr, lr], -- columns
   [ul, mm, lr], [ur, mm, ll]] -- diagonals

-- alle Positionen durch einen Spielzug
moves :: Position -> [Position]
moves position =
  case gameOver position of
    Just _ -> []
    Nothing ->
      map listToPosition (moves' (nextPlayer position) (positionToList position))

-- Spieler sein Spielstein
playerToNoCro :: Player -> NoCro
playerToNoCro PO = O
playerToNoCro PX = X

moves' :: Player -> [NoCro] -> [[NoCro]]
moves' player [] = []
moves' player (f:rest) =
  case f of
    Empty -> ((playerToNoCro player) : rest) :
                (map (\ lis -> Empty : lis) (moves' player rest))
    _ -> map (\ lis -> f : lis) (moves' player rest)

-- ist das Spiel vorbei und wenn ja wer hat gewonnen?
-- Just X: X hat gewonnen
-- Just O: O hat gewonnen
-- Just Empty: alle Felder belegt, unentschieden
-- Nothing: Spiel ist noch nicht vorbei
gameOver :: Position -> Maybe NoCro
gameOver position =
  if won PX position
  then Just X
  else if won PO position
  then Just O
  else if all (\ nocro -> nocro /= Empty) (positionToList position)
  then Just Empty
  else Nothing

-- hat dieser Spieler gewonnen?
won :: Player -> Position -> Bool
won player position =
  any (all ((==) (playerToNoCro player)))
      (axes position)
--  any (\ axis -> all (\ nocro -> nocro == playerToNoCro player) axis)
--      (axes position)


-- Welcher Spieler ist dran?
nextPlayer :: Position -> Player
nextPlayer position =
  let lis = positionToList position
  in if length (filter (\ nocro -> nocro == X) lis) <=
          length (filter (\ nocro -> nocro == O) lis)
      then PX
      else PO
  
data Tree a = Node a [Tree a]
    deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Node n subs) = Node (f n) (map (\ sub -> mapTree f sub) subs)

instance Functor Tree where
  fmap = mapTree

gameTree :: Position -> Tree Position
gameTree position =
  Node position
    (map gameTree (moves position))

