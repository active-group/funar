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
data Pet = -- Typ
    Dog -- Konstruktor (hier 0stellig)
  | Cat
  | Snake
  deriving Show

-- Abseitsregel: Bei mehrzeiligen Konstrukten müssen die
-- Folgezeilen gegenüber der erste Zeile eingerückt sein.

-- Intuition: Kleinbuchstaben = Variablen, Großbuchstaben = Konstanten

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False
isCute pet =
    case pet of
        Dog -> True
        Cat -> True
        Snake -> False
