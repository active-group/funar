{-# LANGUAGE InstanceSigs #-}

module Intro where

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
  deriving Show

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

tuplify :: (a -> b -> c) -> ((a, b) -> c)
tuplify f = \ (a, b) -> f a b