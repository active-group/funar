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

parrot1 = MkParrot "welcome!" 1
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
