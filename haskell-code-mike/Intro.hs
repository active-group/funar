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

-- Tiere auf dem texanischen Highway ...

-- Ein Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
data Liveness = Dead | Alive 

-- Typsyonym
type Weight = Integer

-- Konstruktor heißt genau wie der Typ
data Dillo = Dillo { dilloLiveness :: Liveness,
                     dilloWeight :: Weight }


dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 12 } -- Gürteltier, lebendig, 12kg
dillo2 = Dillo Dead 10 -- Gürteltier, tot, 10kg


