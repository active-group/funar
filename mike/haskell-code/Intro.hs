{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x + 2

double :: Integer -> Integer
double = \x -> x * 2

-- >>> double 21
-- 42


