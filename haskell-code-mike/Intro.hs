{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE UnicodeSyntax #-}
module Intro where

x :: Integer
x = 15 + 22

-- Ein Haustier ist eins der folgenden:
-- - Hund
-- - Katze
-- - Schlange
-- neuer Datentyp
data Pet = Dog | Cat | Snake
  deriving Show

pet1 :: Pet
pet1 = Dog

-- Ist Haustier niedlich?
isCute :: Pet -> Bool
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Tiere auf dem texanischen Highway ...

-- Ein Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
data Liveness = Dead | Alive 
  deriving Show

-- Typsyonym
type Weight = Integer

type Sentence = String

{-
-- Konstruktor heißt genau wie der Typ
data Dillo = Dillo { dilloLiveness :: Liveness,
                     dilloWeight :: Weight }
  deriving Show


dillo1 :: Dillo
dillo1 = Dillo { dilloLiveness = Alive, dilloWeight = 12 } -- Gürteltier, lebendig, 12kg
dillo2 :: Dillo
dillo2 = Dillo Dead 10 -- Gürteltier, tot, 10kg

-- Gürteltier überfahren
-- (: run-over-dillo (dillo -> dillo))
runOverDillo :: Dillo -> Dillo
-- (define run-over-dillo
--   (lambda (dillo)
--     (make-dillo #f (dillo-weight dillo))))
-- runOverDillo = \ dillo -> Dillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo}
-- runOverDillo dillo = Dillo Dead (dilloWeight dillo)
runOverDillo (Dillo _ w) = Dillo Dead w

-- Ein Papagei hat folgende Eigenschaft:
-- - Satz
-- - Gewicht
type Sentence = String

data Parrot = Parrot Sentence Weight
  deriving Show

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei

-}

data Animal = 
    Dillo { dilloLiveness :: Liveness,
            dilloWeight :: Weight }
  | Parrot Sentence Weight 
  deriving Show

dillo1 :: Animal
dillo1 = (Dillo { dilloLiveness = Alive, dilloWeight = 12}) :: Animal

dillo2 :: Animal
dillo2 = Dillo Dead 10

-- Tier überfahren
runOverAnimal :: Animal -> Animal
-- runOverAnimal (Dillo _ weight) = Dillo Dead weight
runOverAnimal (Dillo { dilloWeight = weight }) = Dillo Dead weight
-- runOverAnimal dillo@(Dillo {}) = dillo { dilloLiveness = Dead } -- funktionales Update
runOverAnimal (Parrot _ weight) = Parrot "" weight
{-
runOverAnimal animal =
  case animal of
    (Dillo _ weight) -> Dillo Dead weight
    (Parrot _ weight) -> Parrot "" weight
-}

-- Tier füttern
feedAnimal :: Weight -> (Animal -> Animal)
feedAnimal amount (Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

feedAnimal' :: (Weight, Animal) -> Animal
feedAnimal' (amount, Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal' (amount, Parrot sentence weight) = Parrot sentence (weight + amount)

{-
tuplify :: (Weight -> Animal -> Animal) -> ((Weight, Animal) -> Animal)
tuplify f = \ (weight, animal) -> f weight animal
-}

-- "Was nicht paßt, wird passend gemacht!"
uncurrify :: (a -> b -> c) -> ((a, b) -> c)
uncurrify f = \ (a, b) -> f a b

currify :: ((a, b) -> c) -> (a -> (b -> c))
-- currify f = \ a -> \ b -> f (a, b)
currify = \ f -> \ a -> \ b -> f (a, b)
-- currify f a b = f (a, b)

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste

{-
data ListOfIntegers =
    Empty
  | Cons Integer ListOfIntegers
-}

data ListOf element
  = Empty
  | Cons element (ListOf element)

type ListOfIntegers = ListOf Integer

l1 :: ListOfIntegers
l1 = Cons 1 (Cons 2 (Cons 3 Empty))

-- Elemente einer Liste addieren
listSum :: ListOfIntegers -> Integer
listSum Empty = 0
listSum (Cons first rest) = first + (listSum rest)
