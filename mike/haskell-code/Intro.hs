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