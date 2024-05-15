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

instance Eq Pet where
  (==) :: Pet -> Pet -> Bool
  (==) Dog Dog = True
  (==) Cat Cat = True
  (==) Snake Snake = True
  (==) Fruitfly Fruitfly = True
  (==) _ _ = False

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
listSum [] = 0 -- neutrale Element
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

-- nicht-strikte Auswertung
-- (fast) alle anderen Sprachen: strikt
-- strikt: alle Argumente eines Funktionsaufrufs werden ausgewertet,
-- bevor die Funktion aufgerufen wird
-- Haskell: Ausdrücke werden nur ausgewertet, wenn ihr Ergebnis benötigt wird
-- lazy evaluation
integersFrom :: Integer -> [Integer]
integersFrom n = n : (integersFrom (n+1))

strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n list =
  listFilter (\ n' -> mod n' n /= 0) list

-- >>> strikeMultiples 2 [1,2,3,4,5,6,7,8,9,10]
-- [1,3,5,7,9]

sieve :: [Integer] -> [Integer]
sieve [] = []
-- Unterstellung: x ist eine Primzahl
sieve (x:xs) = x : sieve (strikeMultiples x xs)

primes :: [Integer]
primes = sieve (integersFrom 2)

data Optional a = Null | Result a
  deriving Show

instance Eq a => Eq (Optional a) where
  (==) Null Null = True
  (==) Null (Result _) = False
  (==) (Result _) Null = False
  (==) (Result a1) (Result a2) = a1 == a2

-- data Maybe a = Nothing | Just a

-- Eq a: Constraint

-- Index eines Elements innerhalb einer Liste
listIndex :: Eq a => a -> [a] -> Optional Integer
listIndex element [] = Null
listIndex element (x:xs) =
  if x == element
  then Result 0
  else
    case listIndex element xs of
      Null -> Null
      Result index -> Result (index + 1)

-- >>> listIndex "Mike" ["Felix", "Daniel", "Mike", "Thomas"]
-- Result 2

--- >>> listIndex Dog [Cat, Cat, Fruitfly, Snake, Dog]

-- >>> :type (==)
-- (==) :: Eq a => a -> a -> Bool

-- Eq ist eine Typklasse, denke "Interface"
-- instance: Implementierung einer Typklasse 

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   show :: a -> String

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

-- sinnvolle Typklassen sind für universelle Konzepte / Mathematik

-- besonders nützlich:
-- Halbgruppe, Monoid
-- Funktor, applikative Funktoren, Monaden

-- Algebraische Struktur:

-- 1. eine Menge (Typ)
-- 2. Operation(en)
-- 3. Gleichungen

-- "0 ist das neutrale Element bezüglich +"
-- "1 ist das neutrale Element bezüglich *"

-- Halbgruppe
-- Typ a
-- combine :: a -> a -> a
-- Assoziativgesetz:
-- op (op x y) z = op x (op y z)

-- Beispiele:
-- (x + y) + z == x + (y + z)
-- (x * y) * z == x * (y * z)
-- (x ++ y) ++ z == x ++ (y ++ z)
-- (x && y) && z == x && (y && z)
-- (x || y) || z == x || (y || z)
-- overlay (overlay x y) z == overlay x (overlay y z)

class Semigroup a where
  -- combine (combine x y) z == combine x (combine y z)
  combine :: a -> a -> a

instance Semigroup [b] where
  combine :: [b] -> [b] -> [b]
  --combine list1 list2 = list1 ++ list2
  combine = (++)

-- >>> combine [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

newtype AdditiveIntegers = MkAdditive Integer
newtype MultiplicativeIntegers = MkMultiplicative Integer

instance Semigroup AdditiveIntegers where
  combine :: AdditiveIntegers -> AdditiveIntegers -> AdditiveIntegers
  combine (MkAdditive n1) (MkAdditive n2) = MkAdditive (n1+n2)

instance Semigroup MultiplicativeIntegers where
  combine :: MultiplicativeIntegers -> MultiplicativeIntegers -> MultiplicativeIntegers
  combine (MkMultiplicative n1) (MkMultiplicative n2) = MkMultiplicative (n1+n2)

instance (Semigroup b, Semigroup c) => Semigroup (b, c) where
  combine :: (b, c) -> (b, c) -> (b, c)
  combine (b1, c1) (b2, c2) =
    (combine b1 b2, combine c1 c2 )

instance Semigroup Double where
  combine :: Double -> Double -> Double
  combine n1 n2 = n1 + n2

-- Monoid
-- Halbgruppe a mit zusätzlich:
-- neutral :: a
-- combine neutral x == combine x neutral == x

-- Pfeil andersrum als bei instance!
class Semigroup a => Monoid a where
  -- combine neutral x == combine x neutral == x
  neutral :: a

instance Monoid [b] where
  neutral :: [b]
  neutral = []

instance Monoid AdditiveIntegers where
  neutral = MkAdditive 0

instance Monoid MultiplicativeIntegers where
  neutral = MkMultiplicative 1




-- >>> combine (1.0, 2.0) (3.0, 4.0)
-- (4.0,6.0)

-- Notiz: Funktionskomposition


-- neutrales Element

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
