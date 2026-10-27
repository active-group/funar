{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 43

y :: Integer
y = x * 2

-- >>> y
-- 84

-- Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake -- Aufzählung
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Cat
-- True
-- >>> isCute Snake
-- False

{-
isCute pet =
    case pet of
        Dog -> True
        Cat -> True
        Snake -> False
-}

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht

data Liveness = Alive | Dead
  deriving Show

-- Typalias
type Weight = Integer

-- Record
data Dillo = 
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, 
                   dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = 
--    MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
