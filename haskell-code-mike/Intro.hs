module Intro where

-- Typsignatur
x :: Integer
x = 12

y :: Integer
y = x + 5

f :: Integer -> Integer
-- >>> f 13 
-- 20
f x = x + 7

f' :: Integer -> Integer
f' = \ x -> x + 7

g :: Integer -> Integer -> Integer
g x y = (x + y) * 2

g' :: Integer -> Integer -> Integer
g' = \ x -> \ y -> (x + y) * 2

h x y =
    let d = x - y
    in d * d

tuple3 :: (Pet, Animal, Bool)
tuple3 = (Dog, dillo1, True)
-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
-- data: neuer Datentyp
-- Pet: Typ
-- Dog, Cat, Snake: Konstruktoren
data Pet = Dog | Cat | Snake
  deriving Show

-- data Bool = True | False

-- Ist ein Haustier niedlich?
-- >>> isCute Dog
-- True

-- >>> isCute Snake
-- False

isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Ein Gürteltier hat folgende Eigenschaften:
-- - tot - ODER - lebendig
-- - Gewicht

data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Integer

{-
data Dillo =
    MkDillo { dilloLiveness :: Liveness,
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

-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}

-- Schablone:
-- runOverDillo dillo =
--    MkDillo { dilloLiveness = undefined, dilloWeight = undefined }
--    dilloLiveness dillo  ... dilloWeight dillo

-- runOverDillo dillo =
--   MkDillo {dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
-- runOverDillo dillo =
--    MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w }) =
--    MkDillo Dead w
-- runOverDillo (MkDillo { dilloWeight = w }) =
--    MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
runOverDillo dillo = dillo { dilloLiveness = Dead }

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei
data Parrot = MkParrot String Weight
  deriving Show

parrot1 = MkParrot "Hello" 1
-}

-- Haskell: gemischte Daten müssen in *einer* data-Definition
-- algebraischer Datentyp
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show

{-
interface Animal { 
    ... 
}
class Dillo implements Animal { ... }
class Parrot implements Animal { ... }

class AnimalFactory {
    Animal mkDillo(...) { return new Dillo(...); }
    Animal mkParrot(...) { return new Parrot(...); }
}

-}

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Hello" 1
parrot2 :: Animal
parrot2 = MkParrot "Goodbye" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- Schablone:
-- runOverAnimal (MkDillo liveness weight) = undefined
-- runOverAnimal (MkParrot sentence weight) = undefined

-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}

-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal (MkDillo liveness weight) = MkDillo Dead weight
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- >>> feedAnimal parrot1 2
-- MkParrot "Hello" 3

-- alle Haskell-Funktionen sind einstellig
feedAnimal :: Animal -> (Weight -> Animal)
-- Alias-Pattern
feedAnimal dillo@(MkDillo Dead weight) amount = dillo
feedAnimal (MkDillo Alive weight) amount = MkDillo Alive (weight+amount)
{-
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Dead -> MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
-}
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight + amount)

feedAnimal' :: Weight -> Animal -> Animal
-- Alias-Pattern
feedAnimal' amount dillo@(MkDillo Dead weight)= dillo
feedAnimal' amount (MkDillo Alive weight) = MkDillo Alive (weight + amount)
{-
feedAnimal (MkDillo liveness weight) amount =
    case liveness of
        Dead -> MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
-}
feedAnimal' amount (MkParrot sentence weight) =
  MkParrot sentence (weight + amount)

-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
-- Kleinbuchstaben im Typ: Typvariablen
-- eingebaut als flip
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ b -> \ a -> f a b
swap f b a = f a b

-- eingebaut uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
-- tuplify f = -- f :: a -> b -> c
--  \ (a, b) -> f a b
tuplify f (a, b) = f a b

-- Moses Schönfinkel
-- Haskell B. Curry
-- eingebaut curry
untuplify :: ((a, b) -> c) -> (a -> (b -> c))
--untuplify f a b = f (a, b)
untuplify f = -- f :: (a, b) -> c
     \a -> \b -> f (a, b)

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o bc ab = \a -> bc (ab a)

-- eingebaut unter dem Namen .
-- in Haskell: Namen, die aus Sonderzeichen bestehen, werden infix geschrieben

-- Tupel
feedAnimal'' :: (Weight, Animal) -> Animal
-- >>> feedAnimal''(3, dillo1)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 13}
feedAnimal''(amount, MkDillo liveness weight) =
    case liveness of
        Alive -> MkDillo Alive (weight + amount)
        Dead -> MkDillo liveness weight
feedAnimal''(amount, MkParrot sentence weight) =
    MkParrot sentence (weight+amount)

-- Ein Duschprodukt ist eins der folgenden:
-- - Seife (pH-Wert)
-- - Shampoo (Farbe, Haartyp)

-- Ein Duschgel besteht aus:
-- - prozentualer Anteil Seife
-- - prozentualer Anteil Shampoo

-- Eine geometrische Figur (Shape) ist eins der folgenden:
-- - ein Kreis
-- - Quadrat
-- - eine Überlappung zweier geometrischer Figuren

-- Ist ein Punkt innerhalb einer geometrischen Figur?

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste
data ListOfNumbers =
    Empty