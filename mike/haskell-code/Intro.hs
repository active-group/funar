{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)


x :: Integer
x = 42

double :: Integer -> Integer
-- double x = x * 2
double =
 \ x -> x * 2 -- \ lambda

-- >>> double 42
-- 84

foo :: Integer -> Integer
foo x =
    let y = x * 2
        z = y + 1
    in x + y + z

-- >>> foo 3
-- 16

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
-- Fallunterscheidung/Aufzählung

data Pet =
    Dog 
  | Cat
  | Snake
--  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- eine Gleichung pro Fall ... Pattern-Matching
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- >>> isCute Dog
-- True

-- >>> isCute Cat
-- True

-- >>> isCute Snake
-- False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot
-- - Gewicht

data Liveness =
    Alive 
  | Dead
  deriving Show

-- Typalias
type Weight = Integer

data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8