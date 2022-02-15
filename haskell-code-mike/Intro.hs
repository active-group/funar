module Intro where

x :: Integer
x = 9

y :: Integer
y = x + 5

f :: Integer -> Integer
f n = n + 1

-- Haustiere
data Pet = Dog | Cat | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False