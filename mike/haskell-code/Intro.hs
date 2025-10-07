{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x + 5

-- Zahl verdoppeln
double :: Integer -> Integer
-- double = \ n -> n * 2
double n = n * 2 -- syntaktischer Zucker


-- >>> double 21
-- 42

quadruple :: Integer -> Integer
quadruple n =
    let d = double n
    in double d

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show -- "Zauberspruch"

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

-- Schablone:
{-
isCute pet =
    case pet of -- Verzweigung
      Dog -> undefined
      Cat -> undefined
      Snake -> undefined
-}

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

{-
isCute pet =
  case pet of -- Verzweigung
    Dog -> True
    Cat -> True
    Snake -> False
-}
-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show

type Weight = Integer -- Typ-Alias

{-
-- Record
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive,
                   dilloWeight = 10 }

-- >>> dilloLiveness dillo1
-- Alive

-- totes Gürteltier 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8 -- syntaktischer Zucker

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- viele Schreibweisen:
-- runOverDillo dillo = MkDillo { dilloLiveness = Dead,
--                               dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- Pattern-Matching
-- MkDillo { dilloLiveness = Dead, dilloWeight = 10}
-- MkDillo { dilloLiveness = l,    dilloWeight = w}
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w}) =
--    MkDillo Dead w
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight
-- Kopie bis auf ..., "functional update"
runOverDillo dillo = dillo { dilloLiveness = Dead }
-}

-- Ein Tier ist eins der Folgenden:
-- - Gürteltier -ODER-
-- - Papagei (Eigenschaften: Satz, Gewicht)

data Animal =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive,
                   dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hallo!" 1
parrot2 :: Animal
parrot2 = MkParrot "Tschüss!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal (MkDillo liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight
