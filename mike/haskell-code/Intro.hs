{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 43

y :: Integer
y = x * 2

double :: Integer -> Integer
-- double x = x * 2
double = \ x -> x * 2

-- doublePlus x y = x * 2 + y
doublePlus :: Integer -> Integer -> Integer
doublePlus = \ x -> \ y -> x * 2 + y


-- >>> y
-- 84

-- Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake -- Aufzählung
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Cat
-- True
-- >>> isCute Snake
-- False

{-
isCute pet =
    case pet of
        Dog -> True
        Cat -> True
        Snake -> False
-}

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typalias
type Weight = Integer

{-

-- Record
data Dillo = 
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, 
                   dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = 
--    MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--    MkDillo { dilloLiveness = Dead,        dilloWeight = w}
-- runOverDillo (MkDillo {dilloLiveness = l, dilloWeight = w}) =
--   MkDillo Dead w
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight
-- functional update:
runOverDillo dillo = dillo { dilloLiveness = Dead }

-}

-- Tier (auf dem texanischen Highway) ist eins der folgenden:
-- - Gürteltier ODER
-- - Papagei
data Animal =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "welcome!" 1

parrot2 :: Animal
parrot2 = MkParrot "au revoir!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- 1 Gleichung pro Fall, muß jeweils Konstruktor
runOverAnimal (MkDillo liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- Tier füttern

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> feedAnimal dillo2 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
-- >>> feedAnimal parrot1 5
-- MkParrot "welcome!" 6

feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) foodAmount = -- Alias-Pattern
    case liveness of
        Alive -> MkDillo Alive (weight + foodAmount)
        Dead -> dillo -- MkDillo Dead weight
feedAnimal (MkParrot sentence weight) foodAmount = 
    MkParrot sentence (weight + foodAmount)


feedAnimal' :: Weight -> Animal -> Animal
feedAnimal' foodAmount dillo@(MkDillo liveness weight) =
  -- Alias-Pattern
  case liveness of
    Alive -> MkDillo Alive (weight + foodAmount)
    Dead -> dillo -- MkDillo Dead weight
feedAnimal' foodAmount (MkParrot sentence weight)  =
  MkParrot sentence (weight + foodAmount)

-- Eine (geometrische) Figur ist eins der folgenden:
-- - Kreis ODER
-- - Quadrat ODER
-- - Zusammenlegung von zwei Figuren

-- 1. Datentyp für Figuren
-- 2. eine Funktion, die feststellt, 
-- ob ein Punkt innerhalb oder außerhalb einer Figur liegt

data Point = MkPoint Double Double
  deriving Show

point1 = MkPoint 1 1

point2 = MkPoint 3 3

point3 :: Point
point3 = MkPoint 10 4

data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape}
  deriving Show

circle1 = MkCircle (MkPoint 2 2) 2.0

square1 = MkSquare (MkPoint 3 3) 4.0

overlap1 = MkOverlap circle1 square1

within :: Shape -> Point -> Bool
within (MkCircle (MkPoint centerX centerY) radius) (MkPoint x y) =
  let distanceX = (x - centerX) ^ 2
      distanceY = (y - centerY) ^ 2
      difference = sqrt (distanceX + distanceY)
   in difference <= radius
within (MkSquare (MkPoint squareX squareY) sideLength) (MkPoint x y) =
  let rightTopX = squareX + sideLength
      rightTopY = squareY + sideLength
   in ((x >= squareX) && (x <= rightTopX))
        && ((y >= squareY) && (y <= rightTopY))
within (MkOverlap shape1 shape2) point =
  within shape1 point || within shape2 point

-- Namen aus Sonderzeichen sind implizit infix

-- Eine Liste ist eins der folgenden:
-- - die leere Liste                                   []  ODER
-- - eine Cons-Liste aus erstem Element und Rest-Liste  :

-- 1elementige Liste: 5
list1 :: [Integer]
list1 = 5 : []  -- (cons 5 empty)
-- 2elementige Liste: 2 5
list2 :: [Integer]
list2 = 2 : (5 : [])
list3 :: [Integer]
list3 = [2, 5, 8]
list4 :: [Integer]
list4 = 7 : list3

listSum :: [Integer] -> Integer

-- >>> listSum list4
-- 22
listSum [] = 0
-- listSum (first:rest) =  first + listSum rest
listSum (x:xs) = x + listSum xs
