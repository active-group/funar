{-# LANGUAGE InstanceSigs #-}
module Intro where

-- Kommentar

x :: Integer
x = 43

y :: Integer
y = x * 2 + 23

double :: Integer -> Integer

-- >>> double 12 
-- 24

-- double = \ x -> x * 2
double x = x * 2

quadruple :: Integer -> Integer
-- >>> quadruple 12
-- 48
quadruple x =
 let d = double x
 in double d

quadruple' x = double (double x)

plusDouble :: Integer -> Integer -> Integer
-- plusDouble = \ x -> \ y -> x + 2 * y
plusDouble x y = x + 2 * y

-- Haustier ist eins der folgenden:
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
-- >>> isCute Cat
-- True
-- >>> isCute Snake
-- False

-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot    -UND-
-- - Gewicht
data Liveness = Alive | Dead
  deriving Show

-- Typalias
type Weight = Integer

{-
-- Record
data Dillo = MkDillo { dilloLiveness :: Liveness,
                       dilloWeight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

-- totes Gürteltier, 8kg
dillo2 :: Dillo
dillo2 = MkDillo Dead 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo

-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w }
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight
-- functional update, "Kopie bis auf ..."
runOverDillo dillo = dillo { dilloLiveness = Dead }
-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier -ODER-
-- - Papagei

-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Welcome!" 1

-- Tier überfahren
runOverAnimal :: Animal -> Animal

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- MkParrot "" 1

-- jede Gleichung muß Konstruktor erwähnen
-- Schablone:
-- runOverAnimal (MkDillo liveness weight) = undefined
-- runOverAnimal (MkParrot sentence weight) = undefined
runOverAnimal (MkDillo _liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot _sentence weight) = MkParrot "" weight

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> (feedAnimal dillo2) 5
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Alive -> MkDillo Alive (weight + amount)
        Dead -> MkDillo Dead weight
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - Kreis
-- - Quadrat
-- - eine Überlagerung zweier geometrische Figuren

-- 1. baue einen Datentyp für geometrische Figuren
-- 2. schreibe eine Funktion, die feststellt,
--    ob ein Punkt innerhalb einer Figur ist

-- Typ: Double