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

double = \ x -> x * 2
