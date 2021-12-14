module Intro where

x :: Integer
x = 10

f :: Integer -> Integer
f x = x + 1

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
data Pet = Dog | Cat | Snake 
 deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool 
isCute Dog = True
isCute Cat = True 
isCute Snake = False 

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht
data Liveness = Dead | Alive 
  deriving Show 

type Weight = Integer

{-
data Dillo = Dillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Dillo
dillo2 = Dillo Dead 12

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo = Dillo Dead (dilloWeight dillo)
-- runOverDillo (Dillo { dilloLiveness = l, dilloWeight = w}) = Dillo Dead w
-- runOverDillo (Dillo _ w) = Dillo Dead w
runOverDillo dillo = dillo { dilloLiveness = Dead }

data Parrot = Parrot String Weight
  deriving Show

parrot1 = Parrot "Hello!" 1
parrot2 = Parrot "Goodbye!" 2

runOverParrot :: Parrot -> Parrot
runOverParrot (Parrot _ w) = Parrot "" w
-}

data Animal =
    Dillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | Parrot String Weight
  deriving Show 

dillo1 :: Animal
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 = Dillo Dead 12
parrot1 = Parrot "Hello" 1
parrot2 = Parrot "Goodbye" 2