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
-- Typ: Pet
-- Konstruktoren: Dog / Cat / Snake
data Pet = Dog | Cat | Snake
  deriving Show

-- Ist ein Haustier niedlich?
-- >>> isCute Dog
-- True

-- data Bool = True | False

isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False





