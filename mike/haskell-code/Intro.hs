module Intro where

-- Groß-/Kleinschreibung signifikant
x :: Integer
x = 43

y :: Integer
y = x * 3

f :: Integer -> Integer
-- >>> f 17 
-- 34
f = \ x -> x * 2

f' :: Integer -> Integer
f' x = x * 2

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange

-- neuer Datentyp:
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show

-- Abseitsregel: Bei mehrzeiligen Konstrukten müssen die
-- Folgezeilen gegenüber der erste Zeile eingerückt sein.