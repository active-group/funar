{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

-- Zeilenkommentar

x :: Integer
-- >>> x+15 
-- 57
x = 42

y :: Integer
y = x+3

-- Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange

-- data: neuer Datentyp
data Pet
  = Dog 
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- >>> isCute Cat

-- >>> isCute Snake

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False