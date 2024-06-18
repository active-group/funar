{-# LANGUAGE InstanceSigs #-}
module Intro where

x :: Integer
x = 5

y :: Integer
y = x * 2

-- Zeilenkommentar
-- >>> x + y
-- 15

double :: Integer -> Integer
-- double = \ x -> x * 2
double x = x * 2

-- >>> double 7
-- 14

foo x y =
 let z = x + y
     a = z * 2 
    in z * 4 + a

-- Ein Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange

-- algebraischer Datentyp / Aufzählung
data Pet =
    Dog 
  | Cat
  | Snake
  deriving Show

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
-- eine Gleichung pro Fall
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- >>> isCute Dog
-- True

-- >>> isCute Snake
-- False

-- Gürteltier hat folgende Eigenschaften:
-- - lebendig -ODER- tot -UND-
-- - Gewicht
-- zusammengesetzte Daten

-- Typsynonym
type Weight = Integer

data Liveness = Alive | Dead 
  deriving Show

{-
-- Record
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
              dilloWeight :: Weight }
    deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }

dillo2 :: Dillo
-- dillo2 = MkDillo { dilloLiveness = Dead, dilloWeight = 8 }
dillo2 = MkDillo Dead 8

-- >>> dilloLiveness dillo1
-- Alive
-- >>> dilloWeight dillo2
-- 8

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, 
--                               dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--    MkDillo Dead w
--- runOverDillo (MkDillo { dilloWeight = w }) =
--    MkDillo Dead w
-- runOverDillo (MkDillo _ weight) = MkDillo Dead weight -- - "don't care"
runOverDillo dillo = dillo { dilloLiveness = Dead } -- Kopie von dillo, bis auf ... "functional update"


-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-}

-- Ein Tier (auf dem texanischen Highway) ist eins der folgenden:
-- - Gürteltier -ODER-
-- - Papagei
-- Fallunterscheidung

-- Ein Papagei hat folgende Eigenschaften:
-- - Satz
-- - Gewicht
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
parrot1 = MkParrot "Hello!" 1
parrot2 :: Animal
parrot2 = MkParrot "Goodbye!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _liveness weight) =
  MkDillo Dead weight
runOverAnimal (MkParrot _sentence weight) =
  MkParrot "" weight

-- Datenmodell für die Karten des französischen Blatts

-- Yaron Minsky: "Make illegal states unrepresentable."

feedAnimal dillo@(MkDillo liveness weight) amount = -- alias pattern
  case liveness of
    Alive -> MkDillo liveness (weight + amount)
    Dead -> dillo -- MkDillo liveness weight
feedAnimal (MkParrot sentence weight) amount = 
  MkParrot sentence (weight + amount)