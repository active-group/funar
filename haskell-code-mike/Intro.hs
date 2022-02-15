module Intro where

x :: Integer
x = 9

y :: Integer
y = x + 5

f :: Integer -> Integer
f n = n + 1

-- Haustiere
data Pet = Dog | Cat | Snake
  deriving Show

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
untuplify :: ((a, b) -> c) -> (a -> b -> c)
untuplify f = \ a -> \ b -> f (a, b)

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)


{-
Eine geometrische Figur ("shape") ist:
- Kreis
- Quadrat
- Überlagerungen zweier geometrischer Figuren

1. Datenanalyse durchführen -> Typdefinitionen
2. Funktion schreiben, die für einen Punkt feststellt, ob dieser 
   innerhalb oder außerhalb einer geometrischen Figur liegt
-}