{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 42

double :: Integer -> Integer
-- double x = x * 2
double = \ x -> x * 2

-- >>> double 42
-- 84
