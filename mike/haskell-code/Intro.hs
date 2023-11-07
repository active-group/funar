{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 8

-- Zeilenkommentar

-- >>> x * 2
-- 16

inc :: Integer -> Integer
-- >>> inc 5
-- 6

-- inc x = x + 1
inc = \ x -> x + 1

add :: Integer -> Integer -> Integer
-- add x y = x + y
-- >>> add 5 7
-- 12
add = \ x -> \ y -> x + y

foo :: Integer -> Integer -> Integer
-- >>> foo 5 7
-- 24
foo x y =
    let z = x + y
    in z * 2

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake
  deriving Show

instance Eq Pet where
    (==) :: Pet -> Pet -> Bool
    (==) Dog Dog = True
    (==) Cat Cat = True
    (==) Snake Snake = True
    (==) _ _ = False

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show 

-- Typsynonym
type Weight = Integer

{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
     deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- >>> dilloLiveness dillo1
-- Alive
-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- "functional update"
runOverDillo dillo = dillo { dilloLiveness = Dead }
-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier -ODER-
-- - Papagei
-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo Alive 10

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hallo!" 1
parrot2 :: Animal
parrot2 = MkParrot "Tschüss!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- eine Gleichung pro Fall

-- >>> runOverAnimal parrot1
-- MkParrot "" 1
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- Alias-Pattern
runOverAnimal dillo@(MkDillo liveness weight) = 
    case liveness of
        Dead -> dillo
        Alive -> MkDillo Dead weight
-- runOverAnimal (MkDillo Alive weight) = MkDillo Dead weight
-- runOverAnimal dillo@(MkDillo Dead weight) = dillo
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- Tier füttern

-- >>> feedAnimal dillo1 5

-- nur 1stellige Funktionen
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) amount =
    case liveness of
        Alive -> MkDillo liveness (weight + amount)
        Dead -> dillo
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

-- eingebaut als flip
-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- Kleinbuchstaben auf Typebene: Typvariablen
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f =                              \ weight -> \ animal -> f animal weight 
swap f= \ b -> \ a -> f a b

feedAnimal2 :: Weight -> Animal -> Animal
feedAnimal2 = swap feedAnimal

feed1 :: Animal -> Animal
-- >>> feed1 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 11}
-- >>> feed1 parrot1
-- MkParrot "Hallo!" 2
--- feed1 = feedAnimal2 1
-- feed1 = swap feedAnimal 1
feed1 = flip feedAnimal 1

fdillo1 :: Weight -> Animal
fdillo1 = feedAnimal dillo1

--- >>> feedAnimal'(dillo1, 1)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 11}
feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal'(dillo@(MkDillo liveness weight), amount) = 
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> dillo

feedAnimal'(MkParrot sentence weight, amount) =
    MkParrot sentence (weight + amount)

tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- >>> (tuplify feedAnimal) (dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
tuplify f (a, b) = f a b

-- >>> (untuplify feedAnimal') dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

untuplify :: ((a, b) -> c) -> (a -> (b -> c))
untuplify f a b = f (a,b)

-- eingebaut als curry
schönfinkeln = untuplify
-- eingebaut als uncurry
entschönfinkeln = tuplify

-- Eine geometrische Figur ("Shape") in der Ebene ist eins der folgenden:
-- - ein Kreis
-- - ein Quadrat
-- - eine Überlagerung zweiter geometrischer Figuren

-- Aufgabe:
-- - Datendefinition in Code übersetzen
-- - Funktion schreiben, die feststellt, ob ein Punkt
--   innerhalb oder außerhalb einer geometrischen Figur liegt

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
within overlap@(MkOverlap shape1 shape2) point =
  within shape1 point || within shape2 point

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste
data ListOf a =
    EmptyList 
  | Cons a (ListOf a)

-- leere Liste: []
-- Cons:        :

list1 :: [Integer]
list1 = 5 : []

list2 :: [Integer]
list2 = 2 : 5 : []

list3 :: [Integer]
list3 = 3 : 2 : 5 : []

list4 :: [Integer]
list4 = 8 : list3

list5 :: [Integer]
list5 = [1, 8, 3, 2, 5]

-- Elemente einer Liste addieren
listSum :: [Integer] -> Integer
-- >>> listSum list5
-- 19
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

listFilter :: (a -> Bool) -> [a] -> [a]
listFilter p [] = []
listFilter p (x:xs) =
    if p x 
    then x : (listFilter p xs)
    else listFilter p xs

type List a = [a]

listMap :: (a -> b) -> List a -> List b
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

natsFrom :: Integer -> [Integer]
natsFrom n = n : (natsFrom (n+1))

-- Vielfache einer Zahl aus einer Liste streichen
strikeMultiples :: Integer -> [Integer] -> [Integer]
-- >>> strikeMultiples 2 [1,2,3,4,5,6,7,8]
-- [1,3,5,7]
strikeMultiples x list =
    filter (\x' -> x' `mod` x /= 0) list

-- Sieb des Eratosthenes
sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) =
    x : sieve (strikeMultiples x xs)

primes :: [Integer]
primes = sieve (natsFrom 2)

data Optional a =
    Null
  | Result a
  deriving Show

-- Eq a: Constraint, Eq: Eigenschaft

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- Typklasse, "Interface", Implementierung: instance
-- class Eq a where
--   -- Methode
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}

-- Index eines Elements in einer Liste finden
listIndex :: Eq a => a -> [a] -> Optional Integer
-- >>> listIndex 3 [2,9,4,7,3,5]
-- Result 4
-- >>> listIndex Dog [Cat, Snake, Dog, Snake, Cat]
-- Result 2

listIndex a [] = Null
listIndex a (x:xs) =
    if x == a 
    then Result 0
    else 
        fmap (\index -> index + 1) (listIndex a xs)
{-        
        case listIndex a xs of
            Null -> Null
            Result index -> Result (index + 1)
-}


optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Null = Null
optionalMap f (Result a) = Result (f a)

-- >>> :info Functor
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- aus der Kategorientheorie:
instance Functor Optional where
    fmap = optionalMap

-- >>> :info Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a
--   {-# MINIMAL compare | (<=) #-}
