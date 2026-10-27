{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 43

y :: Integer
y = x * 2

double :: Integer -> Integer
-- double x = x * 2
double = \ x -> x * 2

-- >>> y
-- 84

-- Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake -- Aufzählung
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Cat
-- True
-- >>> isCute Snake
-- False

{-
isCute pet =
    case pet of
        Dog -> True
        Cat -> True
        Snake -> False
-}

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typalias
type Weight = Integer

{-

-- Record
data Dillo = 
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, 
                   dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = 
--    MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--    MkDillo { dilloLiveness = Dead,        dilloWeight = w}
-- runOverDillo (MkDillo {dilloLiveness = l, dilloWeight = w}) =
--   MkDillo Dead w
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight
-- functional update:
runOverDillo dillo = dillo { dilloLiveness = Dead }

-}

-- Tier (auf dem texanischen Highway) ist eins der folgenden:
-- - Gürteltier ODER
-- - Papagei
data Animal =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "welcome!" 1

parrot2 :: Animal
parrot2 = MkParrot "au revoir!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- 1 Gleichung pro Fall, muß jeweils Konstruktor
runOverAnimal (MkDillo liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- Tier füttern

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> feedAnimal dillo2 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
-- >>> feedAnimal parrot1 5
-- MkParrot "welcome!" 6

feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) foodAmount = -- Alias-Pattern
    case liveness of
        Alive -> MkDillo Alive (weight + foodAmount)
        Dead -> dillo -- MkDillo Dead weight
feedAnimal (MkParrot sentence weight) foodAmount = 
    MkParrot sentence (weight + foodAmount)
