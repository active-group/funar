{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 23

-- >>> double 12
-- 24

double :: Integer -> Integer
double x =  -- Abseitsregel
 x * 2

double' :: Integer -> Integer
double' = \ x -> x * 2 

foo :: Integer -> Integer -> Integer
foo = \ x -> \ y ->
    let z = x + y
        a = z + (2*y)
    in (x + y + z) * a

-- Gürteltier hat folgende Eigenschaften:
-- - Lebendigkeit -UND-
-- - Gewicht

-- Lebendigkeit ist eins der folgenden:
-- - Lebendig
-- - Tot

data Liveness = Alive | Dead
  deriving Show

-- Typalias
type Weight = Integer

{-
data Dillo = MkDillo { dilloLiveness :: Liveness, 
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo =
--    MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (MkDillo { dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
runOverDillo (MkDillo _liveness weight) =
    MkDillo Dead weight
-- runOverDillo dillo = dillo { dilloLiveness = Dead } -- functional update

-}

-- data Dillo = MkDillo Liveness Weight

-- algebraischer Datentyp
data Animal =
    MkDillo Liveness Weight
  | MkSnake { snakeLength :: Integer, snakeThickness :: Integer }
  deriving Show

dillo1 = MkDillo Alive 10
dillo2 = MkDillo Dead 8
snake1 = MkSnake 200 5
snake2 = MkSnake 500 20

-- >>> runOverAnimal dillo1 
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal snake1
-- MkSnake {snakeLength = 200, snakeThickness = 0}

runOverAnimal :: Animal -> Animal
-- 1 Gleichung pro Fall
-- Schablone:
-- runOverAnimal (MkDillo liveness weight) = undefined
-- runOverAnimal (MkSnake length thickness) = undefined

runOverAnimal (MkDillo _liveness weight) =
    MkDillo Dead weight
runOverAnimal (MkSnake length _thickness) =
    MkSnake length 0


feedAnimal :: Animal -> (Weight -> Animal)
{-
feedAnimal (MkDillo Alive weight) amount =
  MkDillo Alive (weight+amount)
feedAnimal dillo@(MkDillo Dead _) amount =
      dillo -- MkDillo liveness weight
-}
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of
      Alive -> MkDillo Alive (weight+amount)
      Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkSnake length thickness) amount =
    MkSnake length (thickness+amount)

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- Typvariablen: Kleinbuchstaben
swap :: (a -> b -> c) -> (b -> a -> c)
--swap f = \ b -> \ a -> f a b
swap f b a = f a b

feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal'(dillo@(MkDillo liveness weight), amount) =
  -- Alias-Pattern
  case liveness of
    Alive -> MkDillo Alive (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal'(MkSnake length thickness, amount) =
  MkSnake length (thickness + amount)

-- eingebaut als uncurry
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)
--tuplify f = \ (a, b) -> f a b
entschönfinkeln f (a, b) = f a b

-- Haskell Curry
-- Moses Schönfinkel

-- eingebaut als curry
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
schönfinkeln f a b =  f (a, b)

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - ein Kreis
-- - Quadrat
-- - eine Überlagerung zweiter geometrischer Figuren

-- 1. Datentyp dafür
-- 2. Funktion, die für einen Punkt feststellt, ob dieser
--    innerhalb oder außerhalb einer geometrischen Figur
--    liegt

data Point = MkPoint Double Double

point1 = MkPoint 1 1

point2 = MkPoint 3 3

point3 = MkPoint 10 4

data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape}

circle1 = MkCircle (MkPoint 2 2) 2.0

square1 = MkSquare (MkPoint 3 3) 4.0

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
