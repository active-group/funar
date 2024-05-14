{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 9

-- >>> x * 2
-- 14

inc :: Integer -> Integer
-- inc = \ x -> x + 1
inc x = x + 1 -- syntaktischer Zucker 

-- >>> inc 10
-- 11

foo x y =
    let z = x + y
    in z * 2

-- Ein Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange -ODER-
-- - Fruchtfliege

data Pet =
    Dog
  | Cat
  | Snake 
  | Fruitfly
  deriving Show

isCute :: Pet -> Bool
-- Schablone: eine Gleichung pro Fall
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined
-- isCute Fruitfly = undefined

isCute Dog = True
isCute Cat = True
isCute Snake = True
isCute Fruitfly = False

-- data Bool = True | False

-- >>> isCute Dog
-- True
-- >>> isCute Fruitfly
-- False

-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typ-Alias
type Weight = Integer

{-
data Dillo = 
    MkDillo { -- Konstruktor 
        dilloLiveness :: Liveness, -- Selektor
        dilloWeight :: Weight
    }
    deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Alive 10

-- >>> dilloLiveness dillo1
-- Alive

-- >>> dilloWeight dillo2
-- 10

runOverDillo :: Dillo -> Dillo
--- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = liveness, dilloWeight = w}) = -- Pattern matching
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo {dilloWeight = w}) = MkDillo Dead w
-- runOverDillo dillo = dillo { dilloLiveness = Dead } -- functional update, Kopie bis auf einige Felder
runOverDillo (MkDillo _ weight) = MkDillo Dead weight -- _: "don't care"
-}

{-
data Dillo = MkDillo
  { -- Konstruktor
    dilloLiveness :: Liveness, -- Selektor
    dilloWeight :: Weight
  }
  deriving Show

data Animal =
    Dillo -- 

dillo1 = MkDillo {dilloLiveness = Alive, dilloWeight = 10}
-}

-- algebraischer Datentyp
data Animal =
    MkDillo {
        dilloLiveness :: Liveness,
        dilloWeight :: Weight
    }
  | MkParrot String Weight 
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo {dilloLiveness = Alive, dilloWeight = 10}

dillo2 :: Animal
dillo2 = MkDillo Dead 8 

parrot1 :: Animal
parrot1 = MkParrot "hello" 1
parrot2 :: Animal
parrot2 = MkParrot "goodbye" 2

runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkParrot _ weight) = MkParrot "" weight

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

{-
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of 
        Alive -> MkDillo Alive (weight + amount)
        Dead -> dillo -- MkDillo liveness weight
-}

-- Optimierung
feedAnimal :: Animal -> (Weight -> Animal) -- nur 1stellige Funktionen in Haskell
feedAnimal (MkDillo Alive weight) amount = MkDillo Alive (weight + amount)
feedAnimal dillo@(MkDillo Dead weight) amount = dillo
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

-- >>> (feedAnimal dillo1) 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedAnimal' :: Weight -> Animal -> Animal
feedAnimal' amount (MkDillo Alive weight) = MkDillo Alive (weight + amount)
feedAnimal' amount dillo@(MkDillo Dead weight) = dillo
feedAnimal' amount (MkParrot sentence weight) =
  MkParrot sentence (weight + amount)

feedAnimal'' :: Weight -> Animal -> Animal
feedAnimal'' amount animal = feedAnimal animal amount

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen (Kleinbuchstaben)
-- swap f = \ b -> \ a -> f a b

-- eingebaut als "flip"
swap f b a = f a b

-- >>> swap feedAnimal 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedAnimal''' :: (Weight, Animal) -> Animal
feedAnimal'''(amount, (MkDillo Alive weight)) = MkDillo Alive (weight + amount)
feedAnimal'''(amount, dillo@(MkDillo Dead weight)) = dillo
feedAnimal'''(amount, (MkParrot sentence weight)) =
  MkParrot sentence (weight + amount)
  
-- >>> feedAnimal'''(5, dillo1)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- "entschönfinkeln", eingebaut als uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify f = \ (a, b) -> f a b
tuplify f (a, b) = f a b

tuplify' f tuple = f (fst tuple) (snd tuple)

-- "schönfinkeln", eingebaut als curry
untuplify :: ((a, b) -> c) -> (a -> b -> c)
-- untuplify f a b = f (a, b)
untuplify f =
  \a -> \b -> f (a, b)  

-- Haskell Curry -> to curry
-- Moses Schönfinkel -> schönfinkeln
 
-- >>> tuplify feedAnimal (dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- (2dimensionale Ebene)
-- Eine (geometrische) Figur ("Shape") ist eins der folgenden:
-- - Kreis
-- - Quadrat
-- - eine Überlagerung zweier geometrischer Figuren

-- Entwerfe eine Datenrepräsentation für geometrische Figuren
-- und schreibe eine Funktion, die feststellt, ob ein Punkt
-- innerhalb oder außerhalb einer gegebenen Figur liegt.

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
-- - die leere Liste -ODER-
-- - eine Cons-Liste aus erstem Element und Rest-Liste

data ListOf a =
    EmptyList 
  | Cons a (ListOf a)

-- leere Liste: []
-- Cons:   : (Infix)

list1 :: [Integer]
list1 = 5 : []

list2 :: [Integer]
list2 = 2 : (5 : [])

list3 :: [Integer]
list3 = [2, 5, 8]

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (x:xs) = x + listSum xs

-- >>> listSum [5,12,15]
-- 32

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter _ [] = []
listFilter p (x:xs) = 
  if p x 
  then x : listFilter p xs 
  else listFilter p xs

-- >>> listFilter (\x -> x >= 0) [1,-5, 3]
-- [1,3]

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)
