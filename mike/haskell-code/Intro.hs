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

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig ODER tot
-- - Gewicht

data Liveness = Alive | Dead deriving Show

type Weight = Integer -- Typsynonym

data Dillo = 
    MkDillo { dilloLiveness :: Liveness, -- MkDillo: Konstruktor, dilloLiveness ... Selektoren
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- >>> dilloLiveness dillo1
-- Alive

-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo =
--    MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) = -- Pattern
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _liveness weight) = MkDillo Dead weight -- _: "don't care"
runOverDillo dillo = dillo { dilloLiveness = Dead } -- "functional update", Kopie bis auf ...

-- nur 1stellige Funktionen in Haskell
feedDillo :: Dillo -> Weight -> Dillo
feedDillo dillo amount =
    case dilloLiveness dillo of
        Alive -> dillo { dilloWeight = dilloWeight dillo + amount }
        Dead -> dillo

feedDillo' :: Weight -> Dillo -> Dillo
feedDillo' amount dillo =
    case dilloLiveness dillo of
      Alive -> dillo {dilloWeight = dilloWeight dillo + amount}
      Dead -> dillo

-- Eingaben vertauschen
-- swap :: (Dillo -> Weight -> Dillo) -> (Weight -> Dillo -> Dillo)

-- >>> (swap feedDillo) 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ weight -> \ dillo -> f dillo weight
-- swap f = \b -> \a -> f a b
swap f b a = f a b
-- eingebaut flip

-- Tupel: Ad-hoc zusammengesetzte Daten

-- >>> feedDillo''(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedDillo'' :: (Dillo, Weight) -> Dillo
feedDillo''(dillo, amount) =
  case dilloLiveness dillo of
    Alive -> dillo {dilloWeight = dilloWeight dillo + amount}
    Dead -> dillo
