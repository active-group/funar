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