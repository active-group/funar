module Intro where

x :: Integer
x = 10

f :: Integer -> Integer
f x = x + 1

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake 
deriving Show

