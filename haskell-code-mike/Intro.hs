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
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkParrot _ weight) = MkParrot "" weight
