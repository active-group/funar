{-# LANGUAGE InstanceSigs #-}

module Intro where

-- Groß/Kleinschreibung relevant

x :: Integer
x = 12

y :: Integer
y = x * 3

f :: Integer -> Integer
-- >>> f 5
-- 10
f = \ x -> x * 2

