module Intro where

x :: Integer
x = 9

y :: Integer
y = x + 5

f :: Integer -> Integer
f n = n + 1

-- Haustiere
data Pet = Dog | Cat | Snake
--  deriving Show

instance Eq Pet where
  (==) Cat Cat = True 
  (==) Dog Dog = True 
  (==) Snake Snake = True 
  (==) _ _ = False

-- Ist Haustier niedlich?
-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False
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
-- Record
data Dillo = MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  deriving Show

dillo1 = MkDillo { dilloLiveness = Alive, dilloWeight = 10 }
dillo2 = MkDillo Dead 8

runOverDillo :: Dillo -> Dillo
-- runOverDillo dillo = MkDillo { dilloLiveness = Dead, dilloWeight = dilloWeight dillo }
-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo { dilloLiveness = l, dilloWeight = w}) = 
--     MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w
-- runOverDillo dillo = dillo { dilloLiveness = Dead } -- functional update
runOverDillo (MkDillo { dilloWeight = w}) = MkDillo Dead w
-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei
data Animal =
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot String Weight
  deriving Show 
  
dillo1 :: Animal
dillo1 = MkDillo {dilloLiveness = Alive, dilloWeight = 10}

dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 = MkParrot "Hello" 1

-- Tier überfahren
-- >>> runOverAnimal dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverAnimal dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 8}
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ w) = MkDillo Dead w 
runOverAnimal (MkParrot sentence weight) = MkParrot "" weight

-- Tier füttern
feedAnimal :: Animal -> (Weight -> Animal)
feedAnimal (MkDillo liveness weight) foodWeight =
    case liveness of
        Alive -> MkDillo Alive (weight + foodWeight) 
        Dead -> MkDillo Dead weight
feedAnimal (MkParrot sentence weight) foodWeight =
    MkParrot sentence (weight + foodWeight)

feedAnimal' :: (Animal, Weight) -> Animal
feedAnimal'(MkDillo liveness weight, foodWeight) =
    case liveness of
      Alive -> MkDillo Alive (weight + foodWeight)
      Dead -> MkDillo Dead weight
feedAnimal'(MkParrot sentence weight, foodWeight) =
    MkParrot sentence (weight + foodWeight)

feedAnimal'' :: Weight -> Animal -> Animal
feedAnimal'' foodWeight (MkDillo liveness weight) =
  case liveness of
    Alive -> MkDillo Alive (weight + foodWeight)
    Dead -> MkDillo Dead weight
feedAnimal'' foodWeight (MkParrot sentence weight) =
  MkParrot sentence (weight + foodWeight)

feedAnimal''' foodWeight animal = feedAnimal animal foodWeight

swap :: (b -> a -> c) -> (a -> b -> c)
swap    f     a      b = f b a
-- swap f = \ a -> \ b -> f b a

-- eingebaut: uncurry
tuplify :: (a -> b -> c) -> ((a, b) -> c)
tuplify f = \ (a, b) -> f a b

-- "schönfinkeln" (Moses Schönfinkel)
-- "currifzieren" (Haskell Curry)
-- eingebaut: curry
untuplify :: ((a, b) -> c) -> (a -> (b -> c))
untuplify f = \ a -> \ b -> f (a, b)

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

-- Assoziativgesetz
-- (a + b) + c = a + (b + c)

{-
Eine geometrische Figur ("shape") ist:
- Kreis
- Quadrat
- Überlagerungen zweier geometrischer Figuren

1. Datenanalyse durchführen -> Typdefinitionen
2. Funktion schreiben, die für einen Punkt feststellt, ob dieser 
   innerhalb oder außerhalb einer geometrischen Figur liegt
-}

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste
data ListOf a =
    Empty
  | Cons a (ListOf a)
  deriving Show

list3' :: ListOf Integer
list3' = Cons 1 (Cons 5 (Cons 7 Empty))

list3 :: [Integer]
list3 = [1,5,7]

-- alle Elemente einer Liste addieren
-- >>> listSum list3
-- 13
listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first:rest) = first + (listSum rest)

listMap :: (a -> b) -> [a] -> [b]
listMap f [] = []
listMap f (x:xs) = (f x) : (listMap f xs)

data Optional a =
    Null
  | Result a
  deriving Show

  {-
  data Maybe a =
      Nothing
    | Just a
  -}

-- (==) :: Eq a => a -> a -> Bool
-- Eq a: Constraint

{-
-- Typklasse ("Interface")
class Eq a where
  (==) :: a -> a -> Bool
-}

-- den Index eines Elements in einer Liste bestimmen
listIndex :: Eq a => a -> [a] -> Optional Integer 
listIndex x' [] = Null
listIndex x' (x:xs) = 
    if x == x'
    then Result 0 
    else
      case listIndex x' xs of
        Null -> Null 
        Result index -> Result (index + 1)