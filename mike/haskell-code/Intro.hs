{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

-- Kommentar

x :: Integer
x = 43

y :: Integer
y = x * 2 + 23

double :: Integer -> Integer

-- >>> double 12 
-- 24

-- double = \ x -> x * 2
double x = x * 2

quadruple :: Integer -> Integer
-- >>> quadruple 12
-- 48
quadruple x =
 let d = double x
 in double d

quadruple' x = double (double x)

plusDouble :: Integer -> Integer -> Integer
-- plusDouble = \ x -> \ y -> x + 2 * y
plusDouble x y = x + 2 * y

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   show :: a -> String

-- >>> :info Num
-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a
--   {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
--   	-- Defined in ‘GHC.Num’
-- instance Num Double -- Defined in ‘GHC.Float’
-- instance Num Float -- Defined in ‘GHC.Float’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Num Integer -- Defined in ‘GHC.Num’
-- instance Num Word -- Defined in ‘GHC.Num’

instance Eq Pet where
    (==) :: Pet -> Pet -> Bool
    (==) Dog Dog = True
    (==) Cat Cat = True
    (==) Snake Snake = True
    (==) _ _ = False

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- >>> isCute Cat
-- True
-- >>> isCute Snake
-- False

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot    -UND-
-- - Gewicht
data Liveness = Alive | Dead
  deriving (Show, Eq)

-- Typalias
type Weight = Integer

{-
-- Record
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight
-- functional update, "Kopie bis auf ..."
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
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Welcome!" 1

-- Tier überfahren
runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- jede Gleichung muß Konstruktor erwähnen
-- Schablone:
-- runOverAnimal (MkDillo liveness weight) = undefined
-- runOverAnimal (MkParrot sentence weight) = undefined
runOverAnimal (MkDillo _liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot _sentence weight) = MkParrot "" weight

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> (feedAnimal dillo2) 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
feedAnimal :: Animal -> Weight -> Animal
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Alive -> MkDillo Alive (weight + amount)
        Dead -> MkDillo Dead weight
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- >>> swap feedAnimal 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- eingebaut als "flip"
swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen, kleingeschrieben
-- swap f =                               \ b -> \ a -> f a b
swap f b a = f a b

feedAnimal' :: (Animal, Weight) -> Animal

-- >>> feedAnimal'(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
feedAnimal'(MkDillo liveness weight, amount) =
  case liveness of
    Alive -> MkDillo Alive (weight + amount)
    Dead -> MkDillo Dead weight
feedAnimal'(MkParrot sentence weight, amount) =
  MkParrot sentence (weight + amount)

-- Haskell B Curry ... "to curry"
-- Moses Schönfinkel

-- eingebaut als "uncurry"
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify f =                 \(a, b) -> f a b
entschönfinkeln f (a, b) = f a b

-- eingebaut als "curry"
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
-- untuplify f =                \ a -> \b -> f (a, b)
schönfinkeln f a b = f (a, b)

-- Funktionskomposition
-- eingebaut als .
o :: (b -> c) -> (a -> b) -> (a -> c)
o    f           g        = \ a -> f (g a) 

foo :: Animal -> Animal
-- >>> foo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 15}
foo = runOverAnimal .  (flip feedAnimal 5)

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - Kreis
-- - Quadrat
-- - eine Überlagerung zweier geometrische Figuren

-- 1. baue einen Datentyp für geometrische Figuren
-- 2. schreibe eine Funktion, die feststellt,
--    ob ein Punkt innerhalb einer Figur ist

data Point = MkPoint Double Double
  deriving (Show, Eq)

point1 :: Point
point1 = MkPoint 1 1

point2 :: Point
point2 = MkPoint 3 3

point3 :: Point
point3 = MkPoint 10 4

-- algebraischer Datentyp
data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape} -- Kombinator
  deriving (Show)

circle1 :: Shape
circle1 = MkCircle (MkPoint 2 2) 2.0

square1 :: Shape
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


-- Liste
-- - leere Liste    []
-- - Cons-Liste      : (Infix)

list1 :: [Integer]
list1 = 7 : []

list2 :: [Integer]
list2 = 4 : 7 : []

list3 :: [Integer]
list3 = [5, 4, 7] -- syntaktischer Zucker

list4 :: [Integer]
list4 = 8 : list3

-- eingebaut als sum
listSum :: [Integer] -> Integer
-- >>> listSum list4
-- 24
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

-- eingebaut als map
listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) =  (f x) : (listMap f xs)

integersFrom :: Integer -> [Integer]
integersFrom n = n : integersFrom (n+1)

strikeMultiples :: Integer -> [Integer] -> [Integer]
-- >>> strikeMultiples 2 [2,3,4,5,6,7,8]
-- [3,5,7]
strikeMultiples x list = filter (\y -> mod y x /= 0) list

sieve :: [Integer] -> [Integer]
sieve (x:xs) = x : sieve (strikeMultiples x xs)
sieve [] = []

primes :: [Integer]
primes = sieve (integersFrom 2)

data Optional a =
    Empty
  | Value a
  deriving Show

-- Eq a: Constraint
-- >>> :type (==)
-- (==) :: Eq a => a -> a -> Bool

-- type class / Typklasse ~ Interface
-- instance ~ Implementierung

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- Index eines Elements in einer Liste finden
listIndex :: Eq a => a -> [a] -> Optional Integer

-- >>> listIndex 2 [9, 3, 4, 2, 0] 
-- Value 3
-- >>> listIndex Snake [Cat, Dog, Snake, Dog]
-- Value 2
listIndex x [] = Empty
listIndex x (y:ys) = 
    if x == y
    then Value 0
    else
        case listIndex x ys of
            Empty -> Empty
            Value index -> Value (index + 1)

-- Algebra:

-- Menge/Typ
-- Operationen
-- Gleichungen

-- Magma
-- Typ a
-- combine :: a -> a -> a
-- Gleichungen: nada

-- x + y = y + x

-- Assoziativität:
-- x + (y + z) == (x + y) + z

-- Halbgruppe:
-- Magma +
-- combine x (combine y z) == combine (combine x y) z

class Semigroup a where
  -- combine x (combine y z) == combine (combine x y) z
  combine :: a -> a -> a

-- >>> combine [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]
instance Semigroup [a] where
    combine :: [a] -> [a] -> [a]
    combine = (++)

-- Halbgruppe +
-- neutral :: a
-- combine neutral x == combine x neutral = x

class Semigroup a => Monoid a where
    -- combine neutral x == combine x neutral = x
    neutral :: a

