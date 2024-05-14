{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 7

-- >>> x * 2
-- 14

inc :: Integer -> Integer
-- inc = \ x -> x + 1
inc x = x + 1 -- syntaktischer Zucker 

-- >>> inc 10
-- 11
