module Intro where

-- Typsignatur
x :: Integer
x = 5

y :: Integer
y = x + 12

-- >>> f 17
f a = a + x + y

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake
  deriving Show

-- Ist ein Haustier niedlich?
-- >>> isCute Dog

isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False





