{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 12 + 23 * 2

-- Intuition: "Konstanten" groß, "Variablen" klein
double :: Integer -> Integer
-- double = \ x -> x * 2
double x = x * 2

-- Kommentar
-- >>> double 18 
-- 36

quadruple :: Integer -> Integer
quadruple x =
    let d = double x
    in d * d

-- >>> quadruple 4 
-- 64

data Pet
  = Dog
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- eine Gleichung pro Fall
-- Schablone:
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined

-- isCute pet =
--     case pet of
--         Dog -> True
--         Cat -> True
--         Snake -> False

isCute Dog = True
isCute Cat = True
isCute Snake = False

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- Gürteltier hat folgende Eigenschaften:
-- -- lebendig oder tot
-- -- Gewicht

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

-- >>> dilloLiveness dillo1
-- Alive

-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo =
-- Schablone:
--     MkDillo { dilloLiveness = undefined, dilloWeight = undefined }
--   MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) = -- Pattern
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo { dilloWeight = w}) =
--  MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _liveness weight) = MkDillo Dead weight
runOverDillo dillo = dillo { dilloLiveness = Dead } -- "functional update"


-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
-}


-- Tier auf dem texanischen Highway:
-- - Gürteltier ODER
-- - Klapperschlange

type Thickness = Integer
type Length = Integer

-- algebraischer Datentyp
data Animal
    = MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
    | MkSnake Length Thickness
    deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8
snake1 :: Animal
snake1 = MkSnake 300 10

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkSnake length _) = MkSnake length 0

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal snake1
-- MkSnake 300 0

-- Tier füttern
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Alive -> MkDillo liveness (weight + amount)
        Dead -> MkDillo liveness weight
feedAnimal (MkSnake length thickness) amount =
    MkSnake length (thickness + amount)

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> feedAnimal snake1 7
-- MkSnake 300 17

doubleSum :: Integer -> Integer -> Integer
doubleSum x y = (x+y) * 2

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - Kreis -ODER-
-- - Quadrat -ODER-
-- - Überlagerung zweier geometrischer Figuren

-- 1. Datendefinition in Code übersetzen
-- 2. Funktion schreiben, die für einen Punkt ermittelt, ob dieser
--    innerhalb oder außerhalb einer geometrischen Figur liegt