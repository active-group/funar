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