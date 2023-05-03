module Intro where

-- Groß-/Kleinschreibung signifikant
x :: Integer
x = 43

y :: Integer
y = x * 3

f :: Integer -> Integer
-- >>> f 17 
-- 34
f = \ x -> x * 2

f' :: Integer -> Integer
f' x = x * 2

g :: Integer -> Integer -> Integer
-- g x y =  (x + y) * 2
-- >>> g 5 7
-- 24
g = \ x -> \ y -> (x + y) * 2

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange

-- neuer Datentyp:
data Pet = -- Typ
    Dog -- Konstruktor (hier 0stellig)
  | Cat
  | Snake
  deriving Show

-- Abseitsregel: Bei mehrzeiligen Konstrukten müssen die
-- Folgezeilen gegenüber der erste Zeile eingerückt sein.

-- Intuition: Kleinbuchstaben = Variablen, Großbuchstaben = Konstanten

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False
-- isCute pet =
--    case pet of
--        Dog -> True
--        Cat -> True
--        Snake -> False
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Ein Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Integer

{-
data Dillo = MkDillo { liveness :: Liveness, weight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { liveness = Alive, weight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8 -- Abkürzung

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {liveness = Dead, weight = 10}

-- >>> runOverDillo dillo2
-- MkDillo {liveness = Dead, weight = 8}

-- runOverDillo dillo = MkDillo { liveness = Dead, weight = weight dillo }
-- runOverDillo dillo = MkDillo Dead (weight dillo)
-- runOverDillo (MkDillo { liveness = l, weight = w}) = MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w -- _: don't care
-- runOverDillo (MkDillo { weight = w}) = MkDillo Dead w
-- runOverDillo dillo = dillo { liveness = Dead } -- "functional update", Kopie bis auf ...

-- runOverDillo dillo@(MkDillo l w) = -- Alias-Pattern
--    case l of
--        Dead -> dillo
--        Alive -> MkDillo Dead w

runOverDillo dillo@(MkDillo Dead _) = dillo
runOverDillo (MkDillo Alive w) = MkDillo Dead w

data Parrot = MkParrot String Weight
-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei

data Animal =
    MkDillo { liveness :: Liveness, weight :: Weight}
  | MkParrot String Weight
  deriving Show
-- typisch: gemischte Daten, jeder Fall zusammengesetzte Daten
-- algebraischer Datentyp

dillo1 :: Animal
dillo1 = MkDillo { liveness = Alive, weight = 10}
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Welcome!" 1
parrot2 :: Animal
parrot2 = MkParrot "Goodbye!" 2

runOverAnimal :: Animal -> Animal
-- >>> runOverAnimal dillo1
-- MkDillo {liveness = Dead, weight = 10}

-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal (MkDillo _ w) = MkDillo Dead w
runOverAnimal (MkParrot _ w) = MkParrot "" w

-- In Haskell gibt es nur einstellige Funktionen.

-- >>> feedAnimal dillo1 5
-- MkDillo {liveness = Alive, weight = 15}
feedAnimal :: Animal -> Weight -> Animal
feedAnimal dillo@(MkDillo liveness weight) amount = 
    case liveness of
        Dead -> dillo
        Alive -> MkDillo liveness (weight+amount)
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight+amount)

-- >>> (swap feedAnimal) 5 dillo1
-- MkDillo {liveness = Alive, weight = 15}

-- eingebaut als flip
-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen
-- swap f = \ b -> \ a -> f a b
swap f b a = f a b

feedAnimal' :: (Animal, Weight) -> Animal
-- >>> feedAnimal'(dillo1, 5)
-- MkDillo {liveness = Alive, weight = 15}
feedAnimal' (dillo@(MkDillo liveness weight), amount) =
  case liveness of
    Dead -> dillo
    Alive -> MkDillo liveness (weight + amount)
feedAnimal' ((MkParrot sentence weight), amount) =
  MkParrot sentence (weight + amount)

-- eingebaut als curry
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
schönfinkeln f = \ a -> \b -> f (a, b)

-- eingebaut uncurry
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)
entschönfinkeln f = \ (a, b) -> f a b

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - ein Kreis
-- - ein Quadrat
-- - eine Überlagerung zweier geometrischer Figuren

-- Dafür Datendefinition und Code, außerdem:
-- eine Funktion die für eine geometrische Figur und einen Punkt
-- ermittelt, ob der Punkt innerhalb der außerhalb der geometrischen
-- Figur ist.

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste
{-
data ListOf a =
      Empty 
    | Cons Integer (ListOf a)
    deriving Show
-}

-- In Haskell:
-- leere Liste: []
-- Cons-Liste:  first : rest

-- Summe aller Listenelemente berechnen
listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first : rest) = first + (listSum rest)

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x : xs) = f x : listMap f xs

listFold :: a -> (b -> a -> a) -> [b] -> a 
listFold n f [] = n
listFold n f (x  :                xs) = -- f x (listFold n f xs)
              x `f` (listFold n f xs)

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

-- eingebaut als .