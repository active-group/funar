module Intro where

x :: Integer
x = 10

-- eigener Datentyp
data Pet = Dog | Cat | Snake
  deriving Show

-- Faustregel: Großbuchstaben - Konstante, Kleinbuchstaben - Variable

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Typen und Werte haben unterschiedliche Namensräume

data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Double

{-
-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht
data Dillo = Dillo { dilloLiveness :: Liveness,
                     dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 10}

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = Dillo Dead 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
--- runOverDillo dillo = Dillo Dead (dilloWeight dillo)
-- runOverDillo (Dillo { dilloLiveness = liveness, dilloWeight = weight }) =
--     Dillo Dead weight
-- runOverDillo (Dillo _ weight) = Dillo Dead weight
runOverDillo d = d { dilloLiveness = Dead }
--                   ^^^^^^^ Kopie von dillo, aber dilloLiveness anders

-}

-- Alle Fälle eines Datentyps müssen in eine data-Definition

-- Ein Datentyp, zwei Klassen
-- algebraischer Datentyp
data Animal = Dillo { dilloLiveness :: Liveness,
                      dilloWeight :: Weight }
            | Parrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 10}

dillo2 :: Animal
dillo2 = Dillo Dead 8

parrot1 :: Animal
parrot1 = Parrot "Der Schatz ist Silbersee!" 1
parrot2 :: Animal
parrot2 = Parrot "Tschüss!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo liveness weight) = Dillo Dead weight
runOverAnimal (Parrot sentence weight) = Parrot "" weight

-- Tier füttern

-- Haskell kennt nur 1stellige Funktionen!
-- Hindley-Milner-Typsystem, 1stellige Funktionen, algebraische Datentypen: ML

feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (Dillo Alive weight) amount = Dillo Alive (weight + amount)
feedAnimal (Dillo Dead weight) amount = Dillo Dead weight 
feedAnimal (Parrot sentence weight) amount = Parrot sentence (weight + amount)


