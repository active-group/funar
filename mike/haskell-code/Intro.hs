{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x * 2

-- Zahl verdoppeln
double :: Integer -> Integer

-- >>> double 21
-- 42

-- double = \ x -> x * 2
double x = x * 2 -- syntaktischer Zucker

-- Zahl vervierfachen
quadruple :: Integer -> Integer
-- >>> quadruple 7
-- 28
quadruple x =
    let d = double x
    in double d

-- Ein Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Dog
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
-- 1 Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat:
-- - lebendig -ODER- tot  -UND-
-- - Gewicht
data Liveness =
    Alive | Dead
    deriving Show

type Weight = Integer -- Typsynonym

data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

