{-# LANGUAGE InstanceSigs #-}
module Intro where

-- Signatur:
x :: Integer -- feste Typen: groß, Werte klein
x = 7

y :: Integer
y = 12 * x + 5

-- Eine Zahl verdoppeln
f = \ n -> n * 2


