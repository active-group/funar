{-# LANGUAGE InstanceSigs #-}
module Intro where

import Prelude hiding (Semigroup, Monoid)

x :: Integer
x = 23

-- >>> double 12
-- 24

double :: Integer -> Integer
double x =  -- Abseitsregel
 x * 2

foo x y =
    let z = x + y
        a = z + (2*y)
    in (x + y + z) * a

-- Gürteltier hat folgende Eigenschaften:
-- - Lebendigkeit -UND-
-- - Gewicht

-- Lebendigkeit ist eins der folgenden:
-- - Lebendig
-- - Tot

data Liveness = Alive | Dead
  deriving Show

-- Typalias
type Weight = Integer

data Dillo = MkDillo { dilloLiveness :: Liveness, 
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo =
--    MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
    MkDillo { dilloLiveness = Dead, dilloWeight = w }

z = 12