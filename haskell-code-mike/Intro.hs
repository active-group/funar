module Intro where

-- ghcide

x :: Integer
x = 7

f :: Integer -> Integer
f n = n + 1

data Pet = Hund | Katze | Schlange 
  deriving Show

pet1 :: Pet
pet1 = Hund

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Hund = True
isCute Katze = True
isCute Schlange = False

data Liveness = Dead | Alive 

-- Ein Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
data Dillo = Dillo { dilloLiveness :: Liveness, 
                     dilloWeight :: Integer }

-- GÜrteltier, lebendig, 12kg
dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 12 }
dillo2 :: Dillo
dillo2 = Dillo Dead 10 -- totes Gürteltier, 10kg

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- Schreibweise 1:
-- runOverDillo dillo = Dillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- Schreibweise 2:
runOverDillo (Dillo _ weight) = Dillo { dilloLiveness = Dead, dilloWeight = weight }

-- Papagei hat folgende Eigenschaften:
-- - Satz
-- - Gewicht
data Parrot = Parrot String Integer

parrot1 :: Parrot
parrot1 = Parrot "Hallo!" 10
parror2 :: Parrot
parror2 = Parrot "Der Schatz ist auf der Osterinsel!" 2