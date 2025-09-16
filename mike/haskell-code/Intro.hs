{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x + 2

double :: Integer -> Integer
-- double = \x -> x * 2 -- "Lambda"
double x = x * 2

plus :: Integer -> Integer -> Integer
-- plus x y = x + y
plus = \ x -> \ y -> x + y

-- >>> (plus 23) 42
-- 65

plus23 :: Integer -> Integer

-- >>> plus23 42
-- 65
plus23 = plus 23

-- >>> double 21
-- 42

quadruple :: Integer -> Integer
quadruple x =
    let d = double x
    in double d

-- >>> quadruple 21 
-- 84
 
 {- Blockkommentar
 -}

-- Haustier ist eins der folgenden:
-- - Hund ODER
-- - Katze ODER
-- - Schlange
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show -- hinter jedes data

-- Abseitsregel: Folgezeilen eines mehrzeilgen Konstrukts müssen eingerückt werden

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- Schablone: eine Gleichung pro Fall
-- isCute Dog = undefined
-- isCute Cat = undefined
-- isCute Snake = undefined

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

isCute Dog = True
isCute Cat = True
isCute Snake = False

-- geht auch, aber !/&""%§&!"§"
-- isCute Snake = False
-- isCute otherPet = True
-- isCute _ = True

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig ODER tot  UHND
-- - Gewicht

data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Integer

{-
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8
-- dillo2 = MkDillo Dead 8

runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--     MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w }) =
--  MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w 
-- functional update: Kopie von dillo bis auf { ... }
runOverDillo dillo = dillo { dilloLiveness = Dead }
-} 

-- algebraische Datentypen (ML, OCaml, F#, Haskell, Scala, Swift, ...)

data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "Tschüss!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal (MkDillo _liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot _sentence weight) = MkParrot "" weight

-- Tiere füttern

feedAnimal :: Animal -> (Weight -> Animal)

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
  case liveness of
    Alive -> MkDillo liveness (weight+amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = MkParrot sentence (weight+amount)
