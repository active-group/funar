{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 42

y :: Integer
y = x * 2

-- Zahl verdoppeln
double :: Integer -> Integer


-- >>> double 21
-- 42

-- double = \ x -> x * 2
double x = x * 2 -- syntaktischer Zucker

-- Zahl vervierfachen
quadruple :: Integer -> Integer
-- >>> quadruple 7
-- 28
quadruple x =
    let d = double x
    in double d


doublePlus :: Integer -> Integer -> Integer
doublePlus = \ x -> (\ y -> x * 2 + y)

-- Ein Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange
data Pet =
    Dog
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool

-- >>> isCute Dog
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
-- 1 Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat:
-- - lebendig -ODER- tot  -UND-
-- - Gewicht
data Liveness =
    Alive | Dead
    deriving Show

type Weight = Integer -- Typsynonym

{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
    deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8 -- syntaktischer Zucker

-- >>> dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- >>> dilloLiveness dillo1
-- Alive
-- >>> dilloWeight dillo1
-- 10

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo

--- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = _l, dilloWeight = w}) = MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w}) = MkDillo Dead w
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight
-- functional update:
runOverDillo dillo = dillo { dilloLiveness = Dead } -- "Kopie bis auf ..."
-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei
-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo Alive 10
dillo2 :: Animal
dillo2 = MkDillo Dead 8
parrot1 :: Animal
parrot1 = MkParrot "Welcome!" 1


runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal dillo@MkDillo {} = dillo { dilloLiveness = Dead }  -- Alias-Pattern
--- runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- Tier füttern
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (MkDillo liveness weight) food =
    case liveness of
        Alive -> MkDillo liveness (weight + food)
        Dead -> MkDillo liveness weight
feedAnimal (MkParrot sentence weight) food =
    MkParrot sentence (weight + food)