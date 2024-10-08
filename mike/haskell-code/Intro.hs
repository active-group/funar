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
  deriving Show