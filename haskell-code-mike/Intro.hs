{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Intro where

x :: Integer
x = 15 + 22

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
-- neuer Datentyp
data Pet = Dog | Cat | Snake
  deriving Show

pet1 :: Pet
pet1 = Dog

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False


