module Intro where

{-
Haskell: späte 80er Jahre
getypt, lazy evaluation

Haskell-Standard 2010

inzwischen gibt es nur ghc "Glasgow Haskell Compiler"

Haskell 2010 + viele Erweiterungen

-}

-- Kleinschreibung: Variable
-- Großschreibung : "Konstante"

x :: Integer
x = 10
y :: Integer
y = x + 11
z :: Integer
z = x * y

-- Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange

-- data: eigener, neuer Datentyp
-- Pet: Typ
-- Dog, Cat, Snake: Konstuktoren / Fälle / Klassen
data Pet = Dog | Cat | Snake
  deriving Show -- damit die Werte in der REPL angezeigt werden

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool 
isCute Dog   = True
isCute Cat   = True 
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
data Liveness = Dead | Alive 

-- Typsynonym
type Weight = Double

-- 2 Dinge, die Dillo heißen: Typ, Konstruktor
data Dillo = Dillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 10 } -- lebendiges Gürteltier, 10kg
dillo2 :: Dillo
dillo2 = Dillo Dead 12 -- totes Gürteltier, 12kg