{-# LANGUAGE InstanceSigs #-}
module Intro where

-- Signatur:
x :: Integer -- feste Typen: groß, Werte klein
x = 7

y :: Integer
y = 12 * x + 5

-- Eine Zahl verdoppeln
-- (: f (number -> number))
f :: Integer -> Integer
-- f = \ n -> n * 2 -- \ ist Lambda!
-- f 0 = 0
-- f 1 = 2
-- f 2 = 4
f n = n * 2

-- Ein Haustier ist eins der Folgenden:
-- - Katze -ODER-
-- - Hund -ODER-
-- - Schlange
data Pet = Cat | Dog | Snake
    deriving Show  -- denkt: toString()

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- 1 Gleichung pro Fall (Racket: eine cond-Klausel pro Fall)
-- Pattern Matching: matchen auf best. Werte von Pet
-- Funktionen in Haskell sind _TOTAL_:
-- - zu jedem Input _muss_ es einen Output geben
isCute Cat = True
isCute Dog = True
isCute Snake = False

-- g n = 2 * n

-- Ein Gürteltier hat folgende Eigenschaften:
-- - Gewicht
-- - Lebenszustand
data Liveness = Alive | Dead
    deriving Show

type Weight = Integer