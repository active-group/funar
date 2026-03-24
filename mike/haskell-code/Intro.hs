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
