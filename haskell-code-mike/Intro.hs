module Intro where

x :: Integer
x = 7

y :: Integer
y = x * 12 + 13

f :: Integer -> Integer
-- >>> f 12
-- 24
f = \ n -> n * 2

-- Abkürzung:
f' :: Integer -> Integer
-- >>> f' 12
-- 24
f' n = n * 2

-- Ein Haustier ist eins der folgenden:
-- - Katze - ODER -
-- - Hund - ODER -
-- - Schlange
-- ==> neuer Typ
data Pet = Cat | Dog | Snake
 deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool 
-- 1 Gleichung pro Fall
-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False
isCute Cat = True
isCute Dog = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typsynonym
type Weight = Integer

{-
-- Record-Definition
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
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
-- runOverDillo dillo =
    -- Schablone: dilloLiveness dillo  dilloWeight dillo
--    MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- "functional update"
-- "Kopie von dillo, ein Feld anders"
runOverDillo dillo = dillo { dilloLiveness = Dead }

-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei

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
parrot2 = MkParrot "Goodbye!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- _ : "don't care"
runOverAnimal (MkDillo _ w) = MkDillo Dead w
runOverAnimal (MkParrot _ weight) = MkParrot "" weight

-- nur 1stellige Funktionen in Haskell!

-- Tier füttern
feedAnimal :: Animal -> (Weight -> Animal)
-- >>> (feedAnimal dillo1) 2
-- MkDillo {dilloLiveness = Alive, dilloWeight = 12}
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of
        Dead -> dillo -- MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

feedAnimal' :: Weight -> (Animal -> Animal)
feedAnimal' amount dillo@(MkDillo liveness weight) =
  -- Alias-Pattern
  case liveness of
    Dead -> dillo -- MkDillo liveness weight
    Alive -> MkDillo liveness (weight + amount)
feedAnimal' amount (MkParrot sentence weight) =
  MkParrot sentence (weight + amount)

-- swap :: (Animal -> (Weight -> Animal)) -> (Weight -> (Animal -> Animal))
-- >>> :type swap feedAnimal
-- swap feedAnimal :: Weight -> Animal -> Animal
-- a, b, c: Typvariablen
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ b -> \ a ->  f a b
-- swap f b = \ a -> f a b
-- eingebaut als flip
swap f b a = f a b

feedAnimal'' :: (Weight, Animal) -> Animal
-- >>> feedAnimal'' (1, dillo1)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 11}
feedAnimal''(amount, dillo@(MkDillo liveness weight)) =
  -- Alias-Pattern
  case liveness of
    Dead -> dillo -- MkDillo liveness weight
    Alive -> MkDillo liveness (weight + amount)
feedAnimal''(amount, MkParrot sentence weight) =
  MkParrot sentence (weight + amount)

g :: Integer -> Integer -> Integer
g a b = a + b

tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- >>> (tuplify feedAnimal) (dillo1, 2)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 12}
-- eingebaut als uncurry
tuplify f =
    \ (a, b) -> f a b

-- >>> untuplify feedAnimal'' 2 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 12}
-- eingebaut als curry
untuplify :: ((a, b) -> c) -> (a -> b -> c)
untuplify f =
    \ a -> \ b -> f (a, b)

schönfinkeln = untuplify
entschönfinkeln = tuplify

-- Duschprodukte:
-- - Seife (hat pH-Wert)
-- - Shampoo (hat Haartyp)
-- - Duschgel (50% Seife, 50% Shampoo)

-- Datenanalyse + Datentyp
-- Funktion, die den Seifenanteil berechnet

-- Erweiterung:
-- - Mixtur aus zwei Duschprodukten, beliebige Anteile

-- Mittagspause

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
--   []
-- - eine Cons-Liste aus erstem Element und Rest-Liste
--   first : rest

{-
-- Typkonstruktor
data ListOf element =
    Empty 
  | Cons element (ListOf element)
  deriving Show

list1 :: ListOf Integer
list1 = Cons 5 Empty

list2 :: ListOf Integer
list2 = Cons 2 (Cons 5 Empty)

-}

list1 :: [Integer]
list1 = 5 : []
list2 :: [Integer]
list2 = 2 : (5 : [])
list3 :: [Integer]
list3 = [6, 2, 6]
list4 :: [Integer]
list4 = 7 : list3

