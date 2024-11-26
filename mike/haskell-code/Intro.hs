{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 42

y :: Integer
y = x * 2

double :: Integer -> Integer
-- double = \ x -> x * 2
double x = x * 2

-- >>> double 7
-- 14

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet
  = Dog
  | Cat 
  | Snake 
 deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False
