module Intro where

-- ghcide

x :: Integer
x = 7

f :: Integer -> Integer
f n = n + 1

data Pet = Hund | Katze | Schlange 
  deriving Show

pet1 :: Pet
pet1 = Hund

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Hund = True
isCute Katze = True
isCute Schlange = False


