{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)


x :: Integer
x = 42

double :: Integer -> Integer
-- double x = x * 2
double =
 \ x -> x * 2 -- \ lambda

-- >>> double 42
-- 84

foo :: Integer -> Integer
foo x =
    let y = x * 2
        z = y + 1
    in x + y + z

-- >>> foo 3
-- 16

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
-- Fallunterscheidung/Aufzählung

data Pet =
    Dog 
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- eine Gleichung pro Fall ... Pattern-Matching
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- >>> isCute Dog
-- True

-- >>> isCute Cat
-- True

-- >>> isCute Snake
-- False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness =
    Alive 
  | Dead
  deriving Show

-- Typalias
type Weight = Integer

{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) = MkDillo { dilloLiveness = Dead, dilloWeight = w}
-- runOverDillo (MkDillo { dilloWeight = w }) = MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- "functional update":
runOverDillo dillo = dillo { dilloLiveness = Dead } -- Kopie von dillo, bis auf ...

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-}

-- Tier (auf dem texanischen Highway) ist eins der folgenden:
-- - Gürteltier -ODER-
-- - Papagei
-- gemischte Daten, in Haskell alle in einen Datentyp

-- algebraischer Datentyp

data Animal =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight}
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "goodbye!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- Tier füttern
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of
        Alive -> MkDillo Alive (weight + amount)
        Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = MkParrot sentence (weight + amount)

-- Besser wäre vielleicht feedAnimal :: Weight -> (Animal -> Animal)

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
swap :: (a -> b -> c) -> (b -> a -> c)
swap f = \ b -> \ a -> f a b
    
feedAnimalR :: Weight -> Animal -> Animal
feedAnimalR = swap feedAnimal

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- >>> feedAnimal parrot1 5
-- MkParrot "hello!" 6

-- >>> feedAnimal dillo2 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- Tupel
feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal' (dillo@(MkDillo liveness weight), amount) =
  -- Alias-Pattern
  case liveness of
    Alive -> MkDillo Alive (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal' (MkParrot sentence weight, amount) = MkParrot sentence (weight + amount)

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - ein Kreis
-- - ein Quadrat
-- - eine Überlagerung zweier geometrischer Figuren

-- Aufgabe:
-- 1. Datentyp
-- 2. Funktion, die ermittelt, ob ein Punkt innerhalb oder außerhalb einer geometrischen Figur liegt

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

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest
data ListOf a = -- Typvariable
    Empty 
  | Cons a (ListOf a)

-- 2elementige Liste
loi2 :: ListOf Integer
loi2 = Cons 5 (Cons 7 Empty)

-- die leere Liste: []
-- Cons:            :   (Sonderzeichen sind Infix-Funktionen)

list1 :: [Integer] -- eine Liste aus Integers
list1 = 5 : []

list2 :: [Integer]
list2 = 2 : (5 : [])

list3 :: [Integer]
list3 = [7,2,5]

list4 :: [Integer]
list4 = 8:list3

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first : rest) =  first + listSum rest

-- >>> listSum list4
-- 22

-- (: list-fold (%r (%a %r -> %r) (list-of %a) -> %r))

listFold :: r -> (a -> r -> r) -> [a] -> r
listFold forEmpty forCons [] = forEmpty
listFold forEmpty forCons (x  :                                   xs) =
                           x `forCons` (listFold forEmpty forCons xs)
--  forCons x (listFold forEmpty forCons xs)

-- >>> listFold 0 (+) list4
-- 22

-- >>> listFold 0 (\ x y -> x + y) list4
-- 22
