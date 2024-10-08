{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 43

double :: Integer -> Integer
double x = x * 2

-- >>> double 42
-- 84
