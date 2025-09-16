{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x + 2

double :: Integer -> Integer
-- double = \x -> x * 2
double x = x * 2

-- >>> double 21
-- 42

quadruple :: Integer -> Integer
quadruple x =
    let d = double x
    in double d

-- >>> quadruple 21 
-- 84
 
 {- Blockkommentar
 -}

-- Haustier ist eins der folgenden:
-- - Hund ODER
-- - Katze ODER
-- - Schlange
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show -- hinter jedes data

-- Abseitsregel: Folgezeilen eines mehrzeilgen Konstrukts müssen eingerückt werden

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- Schablone: eine Gleichung pro Fall
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

isCute Dog = True
isCute Cat = True
isCute Snake = False
