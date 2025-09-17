{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 42

y :: Integer
y = x + 2

double :: Integer -> Integer
-- double = \x -> x * 2 -- "Lambda"
double x = x * 2

plus :: Integer -> Integer -> Integer
plus x y = x + y
-- plus = \ x -> \ y -> x + y
-- plus x = \ y -> x + y

-- >>> (plus 23) 42
-- 65

plus23 :: Integer -> Integer

-- >>> plus23 42
-- 65
plus23 = plus 23

-- >>> double 21
-- 42

quadruple :: Integer -> Integer
quadruple x =
    let d = double x
    in double d

-- >>> quadruple 21 
-- 84
 
 {- Blockkommentar
 -}

-- Haustier ist eins der folgenden:
-- - Hund ODER
-- - Katze ODER
-- - Schlange
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show -- hinter jedes data

-- >>> :info Show

instance Eq Pet where
  (==) :: Pet -> Pet -> Bool
  (==) Dog Dog = True
  (==) Cat Cat = True
  (==) Snake Snake = True
  (==) _ _ = False

-- Abseitsregel: Folgezeilen eines mehrzeilgen Konstrukts müssen eingerückt werden

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- Schablone: eine Gleichung pro Fall
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

isCute Dog = True
isCute Cat = True
isCute Snake = False

-- geht auch, aber !/&""%§&!"§"
-- isCute Snake = False
-- isCute otherPet = True
-- isCute _ = True

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig ODER tot  UHND
-- - Gewicht

data Liveness = Dead | Alive
  deriving (Show, Eq)

-- Typsynonym
type Weight = Integer

{-
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8
-- dillo2 = MkDillo Dead 8

runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--     MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w }) =
--  MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w 
-- functional update: Kopie von dillo bis auf { ... }
runOverDillo dillo = dillo { dilloLiveness = Dead }
-} 

-- algebraische Datentypen (ML, OCaml, F#, Haskell, Scala, Swift, ...)

data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving (Eq, Show)

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "Tschüss!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal (MkDillo _liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot _sentence weight) = MkParrot "" weight

-- Tiere füttern

feedAnimal :: Animal -> (Weight -> Animal)

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
  case liveness of
    Alive -> MkDillo liveness (weight+amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = MkParrot sentence (weight+amount)

-- Argumente einer Funktion vertauschen
-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- (lambda (weight) (lambda (animal) (f animal weight)))
swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen
-- swap f =                              \ b -> \ a -> f a b
swap f b a = f a b
-- eingebaut als flip

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)
-- eingebaut unter .

-- Namen aus Sonderzeichen: immer Infix

-- >>> z
-- MkDillo {dilloLiveness = Dead, dilloWeight = 15}
z = (o runOverAnimal (feedAnimal' 5)) dillo1

z' = (runOverAnimal . feedAnimal' 5) dillo1

feedAnimal' :: Weight -> Animal -> Animal
feedAnimal' = swap feedAnimal

-- >>> swap feedAnimal 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- >>> feedAnimal' 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feed1 :: Animal -> Animal
feed1 = feedAnimal' 1

feedDillo1 :: Weight -> Animal
feedDillo1 = feedAnimal dillo1


feedAnimal'' :: (Animal, Weight) -> Animal -- Tupel
feedAnimal''(dillo@(MkDillo liveness weight), amount) =
  -- Alias-Pattern
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal''(MkParrot sentence weight, amount) = MkParrot sentence (weight + amount)

-- eingebaut als uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
tuplify f = \ (a, b) -> f a b

-- >>> (tuplify feedAnimal) (dillo1, 5) -- a = Animal, b = Weight, c = Animal
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- eingebaut als curry (Haskell Curry)
untuplify :: ((a, b) -> c) -> (a -> b -> c)
untuplify f = \a -> \b -> f (a, b)

-- Moses Schönfinkel
schönfinkeln = untuplify

swapTuplified :: ((a, b) -> c) -> ((b, a) -> c)
swapTuplified =  tuplify . swap . untuplify
-- >>> (swap (untuplify feedAnimal'')) 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- Eine geometrische Figuren ("Shape") ist eins der folgenden:
-- - Kreis
-- - Quadrat
-- - Überlagerung zweier geometrischer Figuren

-- 1. Datentyp für geometrische Figuren
-- 2. Funktion, die feststellt, ob ein Punkt innerhalb einer Figur (oder außerhalb) ist

-- type Point = (Double, Double)
data Point = MkPoint Double Double

point1 :: Point
point1 = MkPoint 1 1

point2 :: Point
point2 = MkPoint 3 3

point3 :: Point
point3 = MkPoint 10 4

data Shape
  = MkCircle {center :: Point, radius :: Double}
  | MkSquare {leftBottom :: Point, sideLength :: Double}
  | MkOverlap {shape1 :: Shape, shape2 :: Shape} -- Kombinator

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

-- Listen
data ListOf a =
    Empty
  | Cons a (ListOf a)

-- leere Liste: []
-- Cons       :  :

list1 :: [Integer]
list1 = 5 : []
list2 :: [Integer]
list2 = 8 : (5 : [])
list2' :: [Integer]
list2' = [5, 8]
list3 :: [Integer]
list3 = [3, 5, 8]
list4 :: [Integer]
list4 = 6 : list3

listSum :: [Integer] -> Integer

-- >>> listSum list4
-- 22

-- Liste aufsummieren
listSum [] = 0
listSum (x : xs) = x + listSum xs

listMap :: (a -> b) -> [a] -> [b]

-- >>> listMap runOverAnimal [dillo1, dillo2, parrot1, parrot2]
-- [MkDillo {dilloLiveness = Dead, dilloWeight = 10},MkDillo {dilloLiveness = Dead, dilloWeight = 8},MkParrot "" 1,MkParrot "" 2]

-- eingebaut als map
listMap f [] = []
listMap f (x:xs) = f x : listMap f xs

-- data Maybe a = Nothing | Just a

data Optional a =
    Null
  | Result a
  deriving Show

-- ersten Index eines Elements in einer Liste finden
listIndex :: Eq a => a -> [a] -> Optional Integer -- Constraint

-- >>> listIndex 5 [1,2,7,5,8,5]
-- Result 3
-- >>> listIndex Snake [Dog, Cat, Snake, Cat]
-- Result 2

listIndex element [] = Null
listIndex element (x:xs) = 
  if x == element
  then Result 0
  else
    case listIndex element xs of
      Null -> Null
      Result index -> Result (index + 1)

-- Typklasse ~~ Interface
-- Instanz: Implementierung einer Typklasse

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}
--   	-- Defined in ‘GHC.Classes’
-- instance Eq Integer -- Defined in ‘GHC.Num.Integer’
-- instance Eq Bool -- Defined in ‘GHC.Classes’
-- instance Eq Char -- Defined in ‘GHC.Classes’
-- instance Eq Double -- Defined in ‘GHC.Classes’
-- instance Eq Float -- Defined in ‘GHC.Classes’
-- instance Eq Int -- Defined in ‘GHC.Classes’
-- instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
-- instance Eq Ordering -- Defined in ‘GHC.Classes’
-- instance Eq a => Eq (Solo a) -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j) =>
--          Eq (a, b, c, d, e, f, g, h, i, j)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l, Eq m) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l, Eq m, Eq n) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b) => Eq (a, b) -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
--          Eq (a, b, c, d, e, f)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
--          Eq (a, b, c, d, e, f, g)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
--          Eq (a, b, c, d, e, f, g, h)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
--          Eq (a, b, c, d, e, f, g, h, i)
--   -- Defined in ‘GHC.Classes’
-- instance Eq () -- Defined in ‘GHC.Classes’
-- instance Eq Word -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b) => Eq (Either a b)
--   -- Defined in ‘Data.Either’
-- instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’

-- Kombinatoren, besonders "binäre Kombinatoren"

-- Halbgruppe:
-- Typ a
-- combine :: a -> a -> a
-- Assoziativität
-- combine x (combine y z) == combine (combine x y) z
class Semigroup a where
  -- combine x (combine y z) == combine (combine x y) z
  combine :: a -> a -> a

instance Semigroup Shape where
  combine :: Shape -> Shape -> Shape
  combine = MkOverlap

instance Semigroup [a] where
  combine :: [a] -> [a] -> [a]
  combine = (++)

-- Monoid
-- Halbgruppe a +
-- neutrales Element
-- neutral :: a
-- combine x neutral == combine neutral x == x

-- "ein Monoid muß auch eine Halbgruppe sein"
class Semigroup a => Monoid a where
  -- combine x neutral == combine neutral x == x
  neutral :: a

instance Monoid [a] where
  neutral :: [a]
  neutral = []

-- Kombi:
-- - Typ(en)
-- - Operation(en)
-- - Gleichungen
