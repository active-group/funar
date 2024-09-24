{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 23

double :: Integer -> Integer
double x =  -- Abseitsregel
 x * 2

foo x y =
    let z = x + y
        a = z + (2*y)
    in (x + y + z) * a

-- Gürteltier hat folgende Eigenschaften:
-- - Lebendigkeit -UND-
-- - Gewicht

-- Lebendigkeit ist eins der folgenden:
-- - Lebendig
-- - Tot

data Liveness = Alive | Dead
  deriving Show