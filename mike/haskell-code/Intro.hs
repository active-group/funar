{-# LANGUAGE InstanceSigs #-}

module Intro where

import Prelude hiding (Semigroup)

-- Groß/Kleinschreibung relevant

x :: Integer
x = 12

y :: Integer
y = x * 3

f :: Integer -> Integer
-- >>> f 5
-- 10
-- f = \ x -> x * 2
-- Abkürzung:
f x = x * 2

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet =
    Dog 
  | Cat
  | Snake
  deriving (Show, Eq)

{-
instance Eq Pet where
    (==) :: Pet -> Pet -> Bool
    (==) Dog Dog = True
    (==) Cat Cat = True
    (==) Snake Snake = True
    (==) _ _ = False
-}
-- >>> Dog
-- Dog

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Dog
-- True

-- >>> isCute Snake
-- False

isCute Dog = True 
isCute Cat = True
isCute Snake = False

-- Ein Gürteltier hat folgende Eigenschaften:
-- -- lebendig oder tot
-- -- Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typsynonym
type Weight = Integer

{-
-- Record-Typ
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
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
-- runOverDillo dillo =
--   MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo Dead w
-- runOverDillo (MkDillo l w) = MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w

-- macht Kopie von dillo, bis auf dilloLiveness
-- "functional update"
runOverDillo dillo = dillo { dilloLiveness = Dead }
-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei
-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "Goodbye!" 2

-- >>> dilloLiveness parrot1
-- No match in record selector dilloLiveness

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
-- runOverAnimal (MkParrot _ weight) = MkParrot "" weight

-- Tier füttern

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- gibt nur einstellige Funktionen
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of
        Alive -> MkDillo liveness (weight + amount)
        Dead -> dillo -- MkDillo liveness weight
-- feedAnimal dillo@(MkDillo Alive weight) amount = MkDillo Alive (weight + amount)
-- feedAnimal dillo@(MkDillo Dead weight) amount = -- Alias-Pattern
--   dillo -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = MkParrot sentence (weight + amount)

-- Argumente einer zweistelligen Funktion vertauschen
-- >>> (swap feedAnimal) 5 dillo1
-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- Kleinbuchstaben: Typvariablen
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ b -> \ a -> f a b
swap f b a = f a b
-- eingebaut als "flip"

-- Ein Duschprodukt ist eins der folgenden:
-- - Seife (mit pH-Wert)
-- - Shampoo (mit Haartyp)
-- - Mixtur aus zwei Duschprodukten

-- Datentyp dazu
-- Funktion, die den Seifengehalt berechnet

feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal' (dillo@(MkDillo liveness weight), amount) =
  -- Alias-Pattern
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal' (MkParrot sentence weight, amount) = MkParrot sentence (weight + amount)

-- Haskell Curry
-- Moses Schönfinkel

-- aus einer "gestuften" Funktion eine mit Tupeln machen
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)
entschönfinkeln f = \ (a, b) -> f a b
-- eingebaut als uncurry

-- aus einer Funktion mit Tupeln eine "gestufte" machen
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
schönfinkeln f = \a -> \b -> f (a, b)
-- eingebaut als curry

{-
data ListOf element =
    Empty 
  | Cons Integer (ListOf element)
-}

-- Listen in Haskell:
-- - leere Liste: []
-- - Cons-Listen: cons wird infix als : geschrieben
list1 :: [Integer]
list1 = 5 : []
list2 :: [Integer]
list2 = 8 : (5 : [])
list3 :: [Integer]
list3 = 7 : 8 : 5 : []
list4 :: [Integer]
list4 = 4 : list3

-- Elemente einer Liste addiert
listSum :: [Integer] -> Integer
-- >>> listSum [1,2,3,4]
-- 10
listSum [] = 0
listSum (first : rest) = first + (listSum rest)

type ListOf a = [a]

-- (: list-fold (%b    (%a   %b -> %b)  (list-of %a) -> %b))

listFold ::       b -> (a  -> b -> b) -> ListOf a    -> b
listFold forEmpty forCons [] = forEmpty
listFold forEmpty forCons (first:rest) =
    forCons first (listFold forEmpty forCons rest)

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (first:rest) = (cons (f first) (listMap f rest))

cons = (:)

-- alle Zahlen ab n listen
listFrom :: Integer -> [Integer]
listFrom n = n : (listFrom (n + 1))

-- alle Vielfachen einer Zahl streichen
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n ints =
    filter (\n' -> mod n' n /= 0) ints

-- Sieb des Eratosthenes: erste Zahl ist eine Primzahl
sieve :: [Integer] -> [Integer]
sieve (prime:rest) =
    prime : (sieve (strikeMultiples prime rest)) 

primes :: [Integer]
primes = sieve (listFrom 2)

-- eingebaut als:
-- data Maybe a = Just a | Nothing

data Optional a = -- ein a oder halt keins
    Result a
  | Null
  deriving Show

-- Index eines Elements in einer Liste liefern (0-basiert)
-- >>> listIndex 5 [1,2,3,4,5,6]
-- Result 4
-- links vom =>: Kontext, da stehen Constraints
-- listIndex funktioniert nur für Typen a mit Eq a, "Werte vom Typ a sind vergleichbar"
listIndex :: Eq a => a -> [a] -> Optional Integer
listIndex element [] = Null
listIndex element (first:rest) = 
    if first == element 
    then Result 0
    else
       case listIndex element rest of
        Null -> Null
        Result index -> Result (index + 1)

-- Eq ist eine Typklasse (sowas wie ein Interface)
-- Implementierung einer Typklasse heißt "instance"

-- Verwendung von Typklassen
-- am besten domänenunabhängig, am besten was aus der Mathematik

-- Begriffe aus der Mathematik in der Schulung:
-- ...
-- Funktor, applikativer Funktor, Monade

-- Algebra

-- Assoziativgesetz
-- (a + b) + c = a + (b + c)
-- (a * b) * c = a * (b * c)
-- (a /\ b) /\ c = a /\ (b /\ c)
-- nötig:
-- Typ a
-- op :: a -> a -> a
-- Assoziativgesetz
-- op (op a b) c = op a (op b c)
-- Halbgruppe / Semigroup
class Semigroup a where
  -- op (op a b) c == op a (op b c)
  op :: a -> a -> a

instance Semigroup [a] where
  op :: [a] -> [a] -> [a]
  op list1 list2 = list1 ++ list2

-- neutrales Element
-- Halbgruppe a +
-- neutrales Element n aus a
-- op n a = op a n = a
-- Monoid