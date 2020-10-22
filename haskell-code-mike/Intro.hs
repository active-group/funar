{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE UnicodeSyntax #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 15 + 22

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
-- neuer Datentyp
data Pet = Dog | Cat | Snake
  deriving Show

pet1 :: Pet
pet1 = Dog

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Tiere auf dem texanischen Highway ...

-- Ein Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
data Liveness = Dead | Alive 
  deriving (Show, Eq, Ord)

-- Typsyonym
type Weight = Integer

type Sentence = String

{-
-- Konstruktor heißt genau wie der Typ
data Dillo = Dillo { dilloLiveness :: Liveness,
                     dilloWeight :: Weight }
  deriving Show


dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 12 } -- Gürteltier, lebendig, 12kg
dillo2 :: Dillo
dillo2 = Dillo Dead 10 -- Gürteltier, tot, 10kg

-- Gürteltier überfahren
-- (: run-over-dillo (dillo -> dillo))
runOverDillo :: Dillo -> Dillo
-- (define run-over-dillo
--   (lambda (dillo)
--     (make-dillo #f (dillo-weight dillo))))
-- runOverDillo = \ dillo -> Dillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
-- runOverDillo dillo = Dillo Dead (dilloWeight dillo)
runOverDillo (Dillo _ w) = Dillo Dead w

-- Ein Papagei hat folgende Eigenschaft:
-- - Satz
-- - Gewicht
type Sentence = String

data Parrot = Parrot Sentence Weight
  deriving Show

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei

-}

data Animal = 
    Dillo { dilloLiveness :: Liveness,
            dilloWeight :: Weight }
  | Parrot Sentence Weight 
  deriving Show

dillo1 :: Animal
dillo1 = (Dillo { dilloLiveness = Alive, dilloWeight = 12}) :: Animal

dillo2 :: Animal
dillo2 = Dillo Dead 10

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- runOverAnimal (Dillo _ weight) = Dillo Dead weight
runOverAnimal (Dillo { dilloWeight = weight }) = Dillo Dead weight
-- runOverAnimal dillo@(Dillo {}) = dillo { dilloLiveness = Dead } -- funktionales Update
runOverAnimal (Parrot _ weight) = Parrot "" weight
{-
runOverAnimal animal =
  case animal of
    (Dillo _ weight) -> Dillo Dead weight
    (Parrot _ weight) -> Parrot "" weight
-}

-- Tier füttern
feedAnimal :: Weight -> (Animal -> Animal)
feedAnimal amount (Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

feedAnimal' :: (Weight, Animal) -> Animal
feedAnimal' (amount, Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal' (amount, Parrot sentence weight) = Parrot sentence (weight + amount)

{-
tuplify :: (Weight -> Animal -> Animal) -> ((Weight, Animal) -> Animal)
tuplify f = \ (weight, animal) -> f weight animal
-}

-- "Was nicht paßt, wird passend gemacht!"
uncurrify :: (a -> b -> c) -> ((a, b) -> c)
uncurrify f = \ (a, b) -> f a b

currify :: ((a, b) -> c) -> (a -> (b -> c))
-- currify f = \ a -> \ b -> f (a, b)
currify = \ f -> \ a -> \ b -> f (a, b)
-- currify f a b = f (a, b)

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste

{-
data ListOfIntegers =
    Empty
  | Cons Integer ListOfIntegers
-}

data ListOf element
  = Empty
  | Cons element (ListOf element)

type ListOfIntegers = ListOf Integer

l1 :: ListOfIntegers
l1 = Cons 1 (Cons 2 (Cons 3 Empty))

-- Elemente einer Liste addieren
listSum :: ListOfIntegers -> Integer
listSum Empty = 0
listSum (Cons first rest) = first + (listSum rest)

l1' :: [Integer]
l1' = [1,2,3]
l1'' :: [Integer]
l1'' = 1:(2:(3:[]))

listSum' :: [Integer] -> Integer
listSum' [] = 0
listSum' (first:rest) = first + (listSum' rest)

type List a = [a]

listMap :: (a -> b) -> List a -> List b
listMap _ [] = []
listMap f (first:rest) =
  (f first) : (listMap f rest)

listFold :: b -> (a -> b -> b) -> [a] -> b
listFold x f [] = x
-- listFold x f (first:rest) =
--   f first
--    (listFold x f rest)
listFold x f (first:rest) = first `f` (listFold x f rest)

-- Eine geometrische Figur ist eins der folgenden:
-- - Kreis
-- - Quadrat
-- - eine Überlagerung zweier geometrischer Figuren
--                            ^^^^^^^^^^^^^^^^^^^ Selbstbezug

-- Eine Überlagerung besteht aus:
-- - geometrische Figur
-- - noch 'ne geometrische Figur

-- 1. Aufgabe: Entwickle eine Repräsentation
-- 2. Aufgabe: Funktion, die feststellt, ob ein Punkt
--             innerhalb oder außerhalb einer geometrischen
--             Figur liegt
type Point = (Double, Double)

-- algebraischer Datentyp
data Shape =
    Circle Point Double -- Mittelpunkt, Radius
  | Square Point Double -- Point ist die u.l. Ecke
  | Overlay Shape Shape

-- Ist ein Punkt in einer geometrischen Figur?
pointIsInShape :: Point -> Shape -> Bool
pointIsInShape point (Circle center radius) =
  (distance point center) <= radius
pointIsInShape (px, py) (Square (llCornerX, llCornerY) sideLength) =
  (px >= llCornerX) &&
  (px <= llCornerX + sideLength) &&
  (py >= llCornerY) &&
  (py <= llCornerY + sideLength)
pointIsInShape point (Overlay shape1 shape2) =
  (pointIsInShape point shape1) || (pointIsInShape point shape2)

-- Abstand zwischen zwei Punkten
distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) =
  let sqr x = x * x
  in  sqrt (sqr (x1 - x2) + sqr (y1 - y2))


natsFrom :: Integer -> [Integer]
natsFrom n = n : (natsFrom (n + 1))

strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n [] = []
strikeMultiples n (first:rest) = -- Annahme: n ist Primzahl
  if mod first n == 0
  then strikeMultiples n rest
  else first : (strikeMultiples n rest)

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (first:rest) = -- Annahme: first Primzahl
  first : (sieve (strikeMultiples first rest))

data Map key value = Map [(key, value)]

map1 :: Map String String
map1 = Map [("Mike", "Sperber"), ("Markus", "Spanier")]
map2 :: Map Pet String
map2 = Map [(Cat, "Katze"), (Dog, "Hund")]

data Optional a =
    Absent
  | Present a
  deriving Show

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Absent = Absent
optionalMap f (Present a) = Present (f a)

{-
data Maybe a =
    Nothing
  | Just a
-}

-- Eq key: Constraint / Einschränkung
--         Eigenschaft
mapGet :: Eq key => Map key value -> key -> Optional value
mapGet (Map []) _key = Absent
mapGet (Map ((key', value'):rest)) key =
  if key == key'
  then Present value'
  else mapGet (Map rest) key

-- Typklasse
class HasEquality a where
  equals :: a -> a -> Bool

-- Implementierung einer Typklasse
instance HasEquality Pet where
  equals Dog Dog = True
  equals Cat Cat = True
  equals Snake Snake = True
  equals _ _ = False 

instance Eq Pet where
  Dog == Dog = True
  Cat == Cat = True
  Snake == Snake = True
  _ == _ = False

data Foo = Foo

instance Show Foo where
  show Foo = "Mike"

-- Kombinator:
-- a -> a -> a
-- Beispiele: +, *, overlay, beside, above
-- Assoziativgesetz: (a + b) + c = a + (b + c)
--                   (a * b) * c = a * (b * c)
--                   (overlay (overlay a b) c) = (overlay a (overlay b c))
-- Halbgruppe
class Semigroup a where
  -- Assoziativgesetz:
  -- combine (combine a b) c == combine a (combine b c)
  combine :: a -> a -> a

instance Semigroup Integer where
  combine a b = a + b

instance Semigroup [a] where
  combine = (++)

class Semigroup a => Monoid a where
  -- combine a neutral == combine neutral a == a
  neutral :: a

instance Monoid [a] where
  neutral = []

