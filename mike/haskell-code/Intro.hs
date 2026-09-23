{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 23 + 42

-- >>> x
-- 65

y :: Integer
y = x * 2

-- Pet is one of the following:
-- - dog OR
-- - cat OR
-- - snake
data Pet = Dog | Cat | Snake
  deriving Show

-- Is a pet cute?
isCute :: Pet -> Bool

-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False

-- one equation per case
isCute Dog = True
isCute Cat = True
isCute Snake = False

{-
isCute pet = 
    case pet of
        Dog -> True
        Cat -> True
        Snake -> False
-}

-- An animal (on the Texas highway) is one the following:
-- - armadillo  OR
-- - parrot

-- Armadillo has the following attributes:
-- - alive or dead   AND
-- - weight

data Liveness = Alive | Dead
  deriving Show

-- type alias
type Weight = Integer

data Animal = 
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "welcome!" 1
parrot2 :: Animal
parrot2 = MkParrot "good riddance!" 2

{-
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
    deriving Show

dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Alive 8

-- >>> dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 10}

-- >>> dilloLiveness dillo1
-- Alive
-- >>> dilloWeight dillo2
-- 8

-- run over an armadillo
runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo =
--    MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
--            MkDillo { dilloLiveness = Alive, dilloWeight = 10}
-- runOverDillo (MkDillo { dilloLiveness = l,     dilloWeight = w}) =
--    MkDillo Dead w
-- runOverDillo (MkDillo {dilloWeight = w}) = MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- functional update
runOverDillo dillo = dillo { dilloLiveness = Dead }

-}

runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

runOverAnimal (MkDillo liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- feed animal

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> feedAnimal dillo2 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
-- >>> (feedAnimal parrot1) 5
-- MkParrot "welcome!" 6
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (MkDillo liveness weight) amount = 
    case liveness of
        Alive -> MkDillo liveness (weight + amount)
        Dead -> MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

double :: Integer -> Integer
-- double x = x * 2
-- syntactic sugar:
double = \ x -> x * 2

doublePlus :: Integer -> Integer -> Integer
-- doublePlus x y = x * 2 + y
doublePlus = \ x -> \ y -> x * 2 + y
