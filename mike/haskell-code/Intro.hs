{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 5

y :: Integer
y = x * 2

-- Zeilenkommentar
-- >>> x + y
-- 15

double :: Integer -> Integer
-- double = \ x -> x * 2
double x = x * 2

-- >>> double 7
-- 14

foo x y =
 let z = x + y
     a = z * 2 
    in z * 4 + a

-- Ein Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange

-- algebraischer Datentyp / Aufzählung
data Pet =
    Dog 
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- >>> isCute Dog
-- True

-- >>> isCute Snake
-- False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot -UND-
-- - Gewicht
-- zusammengesetzte Daten

-- Typsynonym
type Weight = Integer

data Liveness = Alive | Dead 
  deriving Show

-- Record
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
-- dillo2 = MkDillo { dilloLiveness = Dead, dilloWeight = 8 }
dillo2 = MkDillo Dead 8