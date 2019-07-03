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
  
-- Spiellogik
-- ^^^^^^^^^^
------------------------------------------------------

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

-- "alles aus der Sicht von X"

-- ungefähre Qualität einer Position
staticScore :: Position -> Integer
staticScore position
  | won PX position = 1
  | won PO position = -1
  | otherwise = 0

staticScores :: Position -> Tree Integer
staticScores position = fmap staticScore (gameTree position)

-- Score maximieren
maximize :: Tree Integer -> Integer
maximize (Node score subs) =
  case subs of
    [] -> score
    (_:_) -> maximum (map minimize subs)

-- Score minimieren
minimize :: Tree Integer -> Integer
minimize  (Node score []) = score
minimize (Node _ subs@(_:_)) = (minimum . map maximize) subs

-- maximize2 = maximum . maximize'
-- minimize2 = minimum . minimize'

{-
Brauchen:
maximum (maximize' (Node score [])) = score
=> maximize' (Node score []) = [score]

maximize' (Node score subs) =
  case subs of
    [] -> [score]
    (_:_) -> map minimize subs -- Definition oben
           = map minimize2 subs -- soll ja gleich werden
           = map (minimum . minimize') subs -- Definition minimize2
           = map minimum (map minimize' subs) -- Funktor
           = mapmin (map minimize' subs) -- Heureka! 
Brauchen: maximum (mapmin ...) = maximum (map minimum ...)
-}

-- nur das Maximum vom Ergebnis zählt!
mapmin :: [[Integer]] -> [Integer]
-- Ergebnis kann auch kürzer sein, solange das Maximum unverändert ist!
mapmin (nums:rest) =
  let min1 = minimum nums
  in min1 : (omitMinima min1 rest)

-- jede Liste ignorieren, deren Minimum potentialMaximum NICHT überschreitet
omitMinima :: Integer -> [[Integer]] -> [Integer]
omitMinima potentialMaximum [] = []
omitMinima potentialMaximum (nums:rest) =
  if minLeq nums potentialMaximum
  then omitMinima potentialMaximum rest
  else let newPotentialMaximum = minimum nums
       in newPotentialMaximum : omitMinima newPotentialMaximum rest  

-- überschreitet das Minimum der Liste das potentielle Maximum NICHT
minLeq :: [Integer] -> Integer -> Bool
minLeq [] potentialMaximum = False
minLeq (n:rest) potentialMaximum =
  (n <= potentialMaximum) ||
    minLeq rest potentialMaximum

evaluate0 :: Position -> Integer
-- evaluate0 position = maximize (fmap staticScore (gameTree position))
evaluate0 = maximize . fmap staticScore . gameTree

-- Eine natürliche Zahl n ist eins der folgenden:
-- - 0
-- - der Nachfolger einer natürlichen Zahl (n - 1)
-- Peano-Definition

-- Baum auf bestimmte Tiefe beschneiden
pruneTree :: Integer -> (Tree a -> Tree a)
pruneTree 0 (Node a subs) = Node a []
pruneTree n (Node a subs) = Node a (map (pruneTree (n - 1)) subs)
-- pruneTree (n + 1) (Node a subs) = Node a (map (pruneTree n) subs)

evaluate1 = maximize . (fmap staticScore) . (pruneTree 5) . gameTree

-- entspricht maximize
maximalMove :: Tree Position -> Position
maximalMove (Node _ subs) =
  let subScores = map (minimize . fmap staticScore) subs
      pairs = zip subs subScores -- Paare aus Teilbaum & Score
      comparePairs (_, score1) (_, score2) = compare score1 score2
      (Node position _, _) = maximumBy comparePairs pairs
  in position

putAt :: Position -> Int -> NoCro -> Position
putAt position index nocro =
  let lis = positionToList position
  in listToPosition ((take index lis) ++ [nocro] ++ (drop (index + 1) lis))

playGame :: Position -> IO ()
playGame position =
  do putStrLn (showPosition position)
     case gameOver position of
      Just X -> do putStrLn "computer wins!"
      Just O -> do putStrLn "human wins!"
      Just Empty -> do putStrLn "tie!"
      Nothing ->
        case nextPlayer position of
          PO ->  do putStrLn "human's turn:"
                    line <- getLine
                    let index = read line
                    let position' = putAt position index O
                    playGame position'
          PX -> do putStrLn "computer's turn"
                   let position' = maximalMove (gameTree position)
                   playGame position'