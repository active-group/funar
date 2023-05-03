module Intro where

-- Groß-/Kleinschreibung signifikant
x :: Integer
x = 43

y :: Integer
y = x * 3

f :: Integer -> Integer
-- >>> f 17 
-- 34
f = \ x -> x * 2
