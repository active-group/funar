module Intro where

-- Typsignatur
x :: Integer
x = 5

y :: Integer
y = x + 12

-- >>> f 17
f a = a + x + y

f' :: Integer -> Integer
f' = \ a -> a + x + y

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
-- Typ: Pet
-- Konstruktoren: Dog / Cat / Snake
data Pet = Dog | Cat | Snake
  deriving Show

-- Ist ein Haustier niedlich?
-- >>> isCute Dog
-- True

-- data Bool = True | False

isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

{-
interface Pet { ... }
class Dog implements Pet { ... }
class Cat implements Pet { ... }
class Snake implements Pet { ... }

Java: 4 Typen

3 Konstruktoren: new Dog, new Cat, new Snake

Pet dog = new Dog()

Haskell:

1 Typ: Pet
-}

-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot
-- - Gewicht

data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Integer

{-
-- Record
data Dillo = MkDillo { dilloLiveness :: Liveness, 
                       dilloWeight :: Weight }
  deriving Show

dillo1 :: Dillo
dillo1 = MkDillo {dilloLiveness = Alive, dilloWeight = 10}
dillo2 :: Dillo
dillo2 = MkDillo Dead 12

-- Faustregel:
-- - Variablen klein
-- - "Konstante" groß

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 12}
-- runOverDillo dillo = 
--    MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- "functional update"
-- runOverDillo dillo = dillo { dilloLiveness = Dead }
-- pattern matching
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) =
--    MkDillo { dilloLiveness = Dead, dilloWeight = w}
runOverDillo (MkDillo _ w) = MkDillo Dead w
-}

-- Ein Tier auf dem texanischen Highway ist eins der folgenden:
-- - Gürteltier
-- - Papagei
-- gemischte Daten: in Haskell alle Fälle in ein "data"

data Animal =
    MkDillo { dilloLiveness :: Liveness, 
              dilloWeight :: Weight }
  | Parrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10}
dillo2 :: Animal
dillo2 = MkDillo Dead 12

parrot1 :: Animal
parrot1 = Parrot "Hallo!" 1
parrot2 :: Animal
parrot2 = Parrot "Goodbye!" 2

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal parrot1
-- Parrot "" 1
runOverAnimal (MkDillo _liveness weight) = MkDillo Dead weight
runOverAnimal (Parrot _ weight) = Parrot "" weight

-- Tier füttern
-- >>> feedAnimal dillo1 5
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> feedAnimal parrot1 7
-- Parrot "Hallo!" 8
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal dillo@(MkDillo liveness weight) amount = -- Alias-Pattern
    case liveness of -- pattern matching
        Dead -> dillo -- MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
feedAnimal (Parrot sentence weight) amount =
    Parrot sentence (weight + amount)

{-
feedAnimal' :: Weight -> (Animal -> Animal)
feedAnimal' amount dillo@(MkDillo liveness weight) =
        case liveness of -- pattern matching
        Dead -> dillo -- MkDillo liveness weight
        Alive -> MkDillo liveness (weight + amount)
feedAnimal' amount (Parrot sentence weight) =
    Parrot sentence (weight + amount)
-}

-- swap :: (Animal -> (Weight -> Animal)) -> (Weight -> (Animal -> Animal))
-- Typvariablen
-- Higher-Order-Funktion, eingebaut als flip
swap :: (a -> b -> c) -> (b -> a -> c)
-- swap f = \ b -> \ a -> f a b
-- swap f = \ b a -> f a b
swap f b a = f a b
-- swap f b a = f a b

feedAnimal' :: Weight -> Animal -> Animal
-- >>> feedAnimal' 5 dillo1
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
feedAnimal' = swap feedAnimal

feedAnimal'' :: (Animal, Weight) -> Animal
-- >>> feedAnimal''(dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- (dillo1, 5) ist ein Tupel, Ad-hoc-zusammengesetzte Daten
feedAnimal''(dillo@(MkDillo liveness weight), amount) =
  -- Alias-Pattern
  case liveness of -- pattern matching
    Dead -> dillo -- MkDillo liveness weight
    Alive -> MkDillo liveness (weight + amount)
feedAnimal''(Parrot sentence weight, amount) =
  Parrot sentence (weight + amount)

-- >>> tuplify (swap feedAnimal) (5, dillo1)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}
-- >>> tuplify feedAnimal (dillo1, 5)
-- MkDillo {dilloLiveness = Alive, dilloWeight = 15}

-- Idee: Haskell Curry
--       Moses Schönfinkel
-- eingebaut als uncurry
tuplify :: (a -> (b -> c)) -> ((a, b) -> c)
tuplify f = \(a, b) -> f a b

-- eingebaut als curry
untuplify :: ((a, b) -> c) -> (a -> (b -> c))
untuplify f a b = f (a, b)

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - ein Kreis
-- - ein Quadrat
-- - eine Überlagerung zweiter geometrischer Figuren

-- Entwickle Datenrepräsentation für geometrische Figuren! 
-- Schreibe eine Funktion, die feststellt, ob ein Punkt in
-- einer geometrischen Figur liegt oder außerhalb.

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste
data ListOfIntegers =
    Empty 
  | Cons Integer ListOfIntegers

list1 = Cons 5 Empty
list2 = Cons 8 (Cons 5 Empty)
list3 = Cons 7 list2