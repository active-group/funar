{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 23 + 42 * 2

y :: Integer
y = x * 2

-- Zahl verdoppeln
double :: Integer -> Integer
-- >>> double 5 
-- 10

-- double = \ x -> x * 2
double x = x * 2 -- syntaktischer Zucker

-- Abseitsregel: Wenn ein Konstrukt über mehrere Zeilen geht,
-- müssen die Folgezeilen gegenüber der ersten Zeile eingerückt
-- sein.

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet = -- data: neuer Datentyp
    Dog -- "Konstruktor"
  | Cat
  | Snake
  deriving Show -- Zauberspruch

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- 1 Gleichung pro Fall
-- Schablone:
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- Pattern-Matching:
isCute Dog = True
isCute Cat = True
isCute Snake = False
-- isCute pet =
--    case pet of
--        Dog -> True
--        Cat -> True
--        Snake -> False

