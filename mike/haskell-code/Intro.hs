{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 10

y :: Integer
y = x + 5

-- >>> x * y
-- 150

f :: Integer -> Integer
-- f x = x * 2
f = \ x -> x * 2 -- \ : Lambda

-- >>> f 12
-- 24

g :: Integer -> (Integer -> Integer)
--g x y = 2 * (x + y)
g = \ x -> \ y -> 2 * (x + y)

g5 :: Integer -> Integer
-- >>> g5 7
-- 24
-- >>> g5 8
-- 26
g5 = g 5

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
-- Fallunterscheidung
-- hier: Aufzählung
data Pet = Dog | Cat | Snake
  deriving Show

instance Eq Pet where
    (==) :: Pet -> Pet -> Bool
    (==) Dog Dog = True
    (==) Cat Cat = True
    (==) Snake Snake = True
    (==) _ _ = False

-- >>> Dog
-- Dog

-- Ist Haustier niedlich?

isCute :: Pet -> Bool

-- >>> isCute Dog
-- True

-- >>> isCute Snake
-- False

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving (Show, Eq, Ord)

{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Integer }
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

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- pattern matching:
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo { dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- "functional update": kopieren bis auf spezifizierte Felder
runOverDillo dillo = dillo { dilloLiveness = Dead }

-}

-- Tier auf dem texanischen Highway:
-- - Gürteltier -ODER-
-- - Papagei

-- Typsynonym
type Weight = Integer

data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Animal
dillo2 = MkDillo Dead 8


parrot1 :: Animal
parrot1 = MkParrot "Hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "Byte!" 2

runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

runOverAnimal (MkDillo _liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot _sentence weight) = MkParrot "" weight

-- Tier füttern

-- feedAnimal (MkDillo Alive weight) amount = MkDillo Alive (weight+amount)
-- feedAnimal d@(MkDillo Dead _weight) _amount = d
feedAnimal :: Animal -> Weight -> Animal
feedAnimal d@(MkDillo liveness weight) amount =
   case liveness of
        Alive -> d { dilloWeight = weight + amount } -- MkDillo liveness (weight + amount)
        Dead -> d -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight+amount)

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> (Animal -> Animal))
-- Typvariablen: Kleinbuchstaben
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ b -> \ a -> f a b
-- swap f b = \ a -> f a b
swap f b a = f a b

-- eingebaut als flip

feedAnimal' :: Weight -> Animal -> Animal
feedAnimal' = swap feedAnimal

feed3 :: Animal -> Animal
-- >>> feed3 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 13}
feed3 = feedAnimal' 3

fdillo1 :: Weight -> Animal
fdillo1 = feedAnimal dillo1

-- >>> fdillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> fdillo1 7
-- MkDillo {dilloLiveness = Alive, dilloWeight = 17}

feedAnimal2 :: (Animal, Weight) -> Animal
feedAnimal2(d@(MkDillo liveness weight), amount) =
  case liveness of
    Alive -> d {dilloWeight = weight + amount} -- MkDillo liveness (weight + amount)
    Dead -> d -- MkDillo liveness weight
feedAnimal2(MkParrot sentence weight, amount) =
  MkParrot sentence (weight + amount)

-- uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
tuplify f =
    \ (a, b) -> f a b

feedAnimal2' :: (Animal, Weight) -> Animal
feedAnimal2' = tuplify feedAnimal

-- curry
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
schönfinkeln f =
    \ a -> \ b -> f (a, b)

feedAnimal'' :: Weight -> Animal -> Animal
feedAnimal'' = flip (schönfinkeln feedAnimal2)

-- Funktionskomposition
-- eingebaut . (Infix)
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

gemein :: Animal -> Animal
gemein = runOverAnimal . (feedAnimal'' 3)

-- >>> feedAnimal2(dillo1, 3)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 1}

{-
- Seife (pH-Wert)

- Shampoo (Haartyp)

- Duschgel besteht (zu gleichen Teilen) aus Seife und Shampoo

- Funktion(en), die den Seifenanteil berechnen

-}

type PH = Float
data Hairtype = Normal | Dandruff | Flauschig | Borstig
  deriving Show
{-
data Soap = MkSoap PH


data Shampoo = MkShampoo Hairtype

data Gel = MkGel Soap Shampoo

soapProp :: Gel -> Float
soapProp _ = 0.5

data Product =
    MkProductSoap Soap
  | MkProductShampoo Shampoo
  | MkProductGel Gel

soapProduct1 = MkProductSoap (MkSoap 7)
-}
data Product =
      MkSoap PH
    | MkShampoo Hairtype
    | MkGel Product Product
   deriving Show

soap1 = MkSoap 7
soap2 = MkSoap 5
shampoo1 = MkShampoo Borstig

shampoo2 = MkShampoo Flauschig

gel1 = MkGel soap1 shampoo1

gel2 = MkGel gel1 shampoo2

gel3 = MkGel gel1 gel2

-- >>> gel3
-- MkGel (MkGel (MkSoap 7.0) (MkShampoo Borstig)) (MkGel (MkGel (MkSoap 7.0) (MkShampoo Borstig)) (MkShampoo Flauschig))

-- Eine geometrische Figur ("shape") ist eins der folgenden:
-- - ein Kreis
-- - ein Quadrat
-- - die Überlagerung zweier geometrischer Figuren

-- Schreibe eine Funktion, die für einen gegebenen Punkt
-- ermittelt, ob er innerhalb oder außerhalb einer Figur
-- liegt.

-- >>> sqrt 2
-- 1.4142135623730951

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

data Position = Inside | Outside
  deriving Show

within :: Shape -> Point -> Position
within (MkCircle (MkPoint centerX centerY) radius) (MkPoint x y) =
  let distanceX = (x - centerX) ^ 2
      distanceY = (y - centerY) ^ 2
      difference = sqrt (distanceX + distanceY)
   in if difference <= radius then Inside else Outside
within (MkSquare (MkPoint squareX squareY) sideLength) (MkPoint x y) =
  let rightTopX = squareX + sideLength
      rightTopY = squareY + sideLength
   in if ((x >= squareX) && (x <= rightTopX))
        && ((y >= squareY) && (y <= rightTopY))
      then Inside
      else Outside
within overlap@(MkOverlap shape1 shape2) point =
    case (within shape1 point, within shape2 point) of
        (Outside, Outside) -> Outside
        _ -> Inside
{-  case within shape1 point of
    Inside -> Inside
    Outside ->
        case within shape2 point of
            Inside -> Inside
            Outside -> Outside
-}

-- Eine Liste ist eins der folgenden:
-- - die leere Liste:  []
-- - eine Cons-Liste aus erstem Element und Rest-Liste:  first : rest
{-
data ListOf element =
    Empty
  | Cons element (ListOf element)
-}

list1 :: [Integer]
list1 = 5 : []

list2 :: [Integer]
list2 = 8 : (5 : [])

list3 :: [Integer]
list3 = [3, 8, 5]
list4 :: [Integer]
list4 = 6 : list3

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first : rest) = 
    first + (listSum rest)

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (first:rest) =
    f first : listMap f rest

data Optional a =
    Empty
  | Result a
  deriving Show

{-
data Maybe a = Nothing | Just a
-}

-- Eq a: Constraint
-- "die Werte von a sind vergleichbar"

-- Eq: Typklasse ("Interface")
-- Implementierung von Interface: Instanz
-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   {-# MINIMAL (==) | (/=) #-}
--   	-- Defined in ‘GHC.Classes’
-- instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
-- instance Eq Word -- Defined in ‘GHC.Classes’
-- instance Eq a => Eq (Solo a) -- Defined in ‘GHC.Classes’
-- instance Eq Ordering -- Defined in ‘GHC.Classes’
-- instance Eq Int -- Defined in ‘GHC.Classes’
-- instance Eq Float -- Defined in ‘GHC.Classes’
-- instance Eq Double -- Defined in ‘GHC.Classes’
-- instance Eq Char -- Defined in ‘GHC.Classes’
-- instance Eq Bool -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l, Eq m, Eq n, Eq o) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l, Eq m, Eq n) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l, Eq m) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l, m)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k, Eq l) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k, l)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j, Eq k) =>
--          Eq (a, b, c, d, e, f, g, h, i, j, k)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i,
--           Eq j) =>
--          Eq (a, b, c, d, e, f, g, h, i, j)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) =>
--          Eq (a, b, c, d, e, f, g, h, i)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) =>
--          Eq (a, b, c, d, e, f, g, h)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) =>
--          Eq (a, b, c, d, e, f, g)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) =>
--          Eq (a, b, c, d, e, f)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (a, b, c, d, e)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c, Eq d) => Eq (a, b, c, d)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b, Eq c) => Eq (a, b, c)
--   -- Defined in ‘GHC.Classes’
-- instance (Eq a, Eq b) => Eq (a, b) -- Defined in ‘GHC.Classes’
-- instance Eq () -- Defined in ‘GHC.Classes’
-- instance Eq Integer -- Defined in ‘GHC.Num.Integer’
-- instance Eq a => Eq (Maybe a) -- Defined in ‘GHC.Maybe’
-- instance (Eq a, Eq b) => Eq (Either a b)
--   -- Defined in ‘Data.Either’

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   showsPrec :: Int -> a -> ShowS
--   show :: a -> String
--   showList :: [a] -> ShowS
--   {-# MINIMAL showsPrec | show #-}
--   	-- Defined in ‘GHC.Show’
-- instance [safe] Show a => Show (Optional a)
--   -- Defined at /Users/sperber/data/active-group/schulung/isaqb/funar/funar-2023-12-hh/mike/haskell-code/Intro.hs:357:12
-- instance [safe] Show Position
--   -- Defined at /Users/sperber/data/active-group/schulung/isaqb/funar/funar-2023-12-hh/mike/haskell-code/Intro.hs:297:12
-- instance [safe] Show Product
--   -- Defined at /Users/sperber/data/active-group/schulung/isaqb/funar/funar-2023-12-hh/mike/haskell-code/Intro.hs:250:13
-- instance [safe] Show Hairtype
--   -- Defined at /Users/sperber/data/active-group/schulung/isaqb/funar/funar-2023-12-hh/mike/haskell-code/Intro.hs:227:12
-- instance [safe] Show Animal
--   -- Defined at /Users/sperber/data/active-group/schulung/isaqb/funar/funar-2023-12-hh/mike/haskell-code/Intro.hs:114:12
-- instance [safe] Show Liveness
--   -- Defined at /Users/sperber/data/active-group/schulung/isaqb/funar/funar-2023-12-hh/mike/haskell-code/Intro.hs:66:12
-- instance [safe] Show Pet
--   -- Defined at /Users/sperber/data/active-group/schulung/isaqb/funar/funar-2023-12-hh/mike/haskell-code/Intro.hs:41:12
-- instance Show a => Show [a] -- Defined in ‘GHC.Show’
-- instance Show Word -- Defined in ‘GHC.Show’
-- instance Show a => Show (Solo a) -- Defined in ‘GHC.Show’
-- instance Show RuntimeRep -- Defined in ‘GHC.Show’
-- instance Show Ordering -- Defined in ‘GHC.Show’
-- instance Show a => Show (Maybe a) -- Defined in ‘GHC.Show’
-- instance Show Integer -- Defined in ‘GHC.Show’
-- instance Show Int -- Defined in ‘GHC.Show’
-- instance Show Char -- Defined in ‘GHC.Show’
-- instance Show Bool -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k, Show l, Show m, Show n, Show o) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k, Show l, Show m, Show n) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k, Show l, Show m) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k, l, m)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k, Show l) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k, l)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j, Show k) =>
--          Show (a, b, c, d, e, f, g, h, i, j, k)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i, Show j) =>
--          Show (a, b, c, d, e, f, g, h, i, j)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h, Show i) =>
--          Show (a, b, c, d, e, f, g, h, i)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f, Show g,
--           Show h) =>
--          Show (a, b, c, d, e, f, g, h)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f,
--           Show g) =>
--          Show (a, b, c, d, e, f, g)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e, Show f) =>
--          Show (a, b, c, d, e, f)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d, Show e) =>
--          Show (a, b, c, d, e)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c, Show d) => Show (a, b, c, d)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b, Show c) => Show (a, b, c)
--   -- Defined in ‘GHC.Show’
-- instance (Show a, Show b) => Show (a, b) -- Defined in ‘GHC.Show’
-- instance Show () -- Defined in ‘GHC.Show’
-- instance Show Float -- Defined in ‘GHC.Float’
-- instance Show Double -- Defined in ‘GHC.Float’
-- instance (Show a, Show b) => Show (Either a b)
--   -- Defined in ‘Data.Either’

-- Index eines Elements in einer Liste finden
listIndex :: Eq a => a -> [a] -> Optional Integer

-- >>> listIndex 7 [3, 5, 7, 8]
-- Result 2

-- >>> listIndex Snake [Dog, Cat, Snake]
-- Result 2

listIndex a [] = Empty
listIndex a (first:rest) =
    if first == a
    then Result 0 
    else 
        case listIndex a rest of
          Empty -> Empty
          Result index -> Result (index + 1)


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
-- instance Num Word -- Defined in ‘GHC.Num’
-- instance Num Integer -- Defined in ‘GHC.Num’
-- instance Num Int -- Defined in ‘GHC.Num’
-- instance Num Float -- Defined in ‘GHC.Float’
-- instance Num Double -- Defined in ‘GHC.Float’

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
--   	-- Defined in ‘GHC.Classes’
-- instance [safe] Ord Liveness
--   -- Defined at /Users/sperber/data/active-group/schulung/isaqb/funar/funar-2023-12-hh/mike/haskell-code/Intro.hs:73:23
-- instance Ord a => Ord [a] -- Defined in ‘GHC.Classes’
-- instance Ord Word -- Defined in ‘GHC.Classes’
-- instance Ord a => Ord (Solo a) -- Defined in ‘GHC.Classes’
-- instance Ord Ordering -- Defined in ‘GHC.Classes’
-- instance Ord Int -- Defined in ‘GHC.Classes’
-- instance Ord Float -- Defined in ‘GHC.Classes’
-- instance Ord Double -- Defined in ‘GHC.Classes’
-- instance Ord Char -- Defined in ‘GHC.Classes’
-- instance Ord Bool -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k, Ord l, Ord m, Ord n, Ord o) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k, Ord l, Ord m, Ord n) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k, l, m, n)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k, Ord l, Ord m) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k, l, m)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k, Ord l) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k, l)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j, Ord k) =>
--          Ord (a, b, c, d, e, f, g, h, i, j, k)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i, Ord j) =>
--          Ord (a, b, c, d, e, f, g, h, i, j)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g, Ord h,
--           Ord i) =>
--          Ord (a, b, c, d, e, f, g, h, i)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g,
--           Ord h) =>
--          Ord (a, b, c, d, e, f, g, h)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f, Ord g) =>
--          Ord (a, b, c, d, e, f, g)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) =>
--          Ord (a, b, c, d, e, f)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d, Ord e) => Ord (a, b, c, d, e)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c, Ord d) => Ord (a, b, c, d)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b, Ord c) => Ord (a, b, c)
--   -- Defined in ‘GHC.Classes’
-- instance (Ord a, Ord b) => Ord (a, b) -- Defined in ‘GHC.Classes’
-- instance Ord () -- Defined in ‘GHC.Classes’
-- instance Ord Integer -- Defined in ‘GHC.Num.Integer’
-- instance Ord a => Ord (Maybe a) -- Defined in ‘GHC.Maybe’
-- instance (Ord a, Ord b) => Ord (Either a b)
--   -- Defined in ‘Data.Either’
