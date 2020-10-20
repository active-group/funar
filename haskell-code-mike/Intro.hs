{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE UnicodeSyntax #-}
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
  deriving Show

-- Typsyonym
type Weight = Integer

-- Konstruktor heißt genau wie der Typ
data Dillo = Dillo { dilloLiveness :: Liveness,
                     dilloWeight :: Weight }
  deriving Show


dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 12 } -- Gürteltier, lebendig, 12kg
dillo2 :: Dillo
dillo2 = Dillo Dead 10 -- Gürteltier, tot, 10kg

-- Gürteltier überfahren
-- (: run-over-dillo (dillo -> dillo))
runOverDillo :: Dillo -> Dillo
-- (define run-over-dillo
--   (lambda (dillo)
--     (make-dillo #f (dillo-weight dillo))))
-- runOverDillo = \ dillo -> Dillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
runOverDillo = λ dillo -> Dillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
-- runOverDillo dillo = Dillo Dead (dilloWeight dillo)
-- runOverDillo (Dillo l w) = Dillo Dead w

