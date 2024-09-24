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

{-
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
-- runOverDillo (MkDillo { dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
runOverDillo (MkDillo _liveness weight) =
    MkDillo Dead weight
-- runOverDillo dillo = dillo { dilloLiveness = Dead } -- functional update

-}

-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkSnake { snakeLength :: Integer, snakeThickness :: Integer }
  deriving Show

dillo1 = MkDillo Alive 10
dillo2 = MkDillo Dead 8
snake1 = MkSnake 200 5
snake2 = MkSnake 500 20

-- >>> runOverAnimal dillo1 
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal snake1
-- MkSnake {snakeLength = 200, snakeThickness = 0}

runOverAnimal :: Animal -> Animal
-- 1 Gleichung pro Fall
-- Schablone:
-- runOverAnimal (MkDillo liveness weight) = undefined
-- runOverAnimal (MkSnake length thickness) = undefined

runOverAnimal (MkDillo _liveness weight) =
    MkDillo Dead weight
runOverAnimal (MkSnake length _thickness) =
    MkSnake length 0


feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of
      Alive -> MkDillo Alive (weight+amount)
      Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkSnake length thickness) amount =
    MkSnake length (thickness+amount)

