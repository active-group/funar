{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x * 2 + 23

double :: Integer -> Integer
double = \ x -> x * 2
