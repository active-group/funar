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
dillo2 :: Animal
dillo2 = Dillo Dead 12
parrot1 :: Animal
parrot1 = Parrot "Hello" 1
parrot2 :: Animal
parrot2 = Parrot "Goodbye" 2

runOverAnimal :: Animal -> Animal
runOverAnimal (Dillo _ w) = Dillo Dead w
runOverAnimal (Parrot _ w) = Parrot "" w

feedAnimal :: Weight -> (Animal -> Animal)
feedAnimal amount (Dillo Alive weight) = Dillo Alive (weight + amount)
feedAnimal amount dillo@(Dillo Dead _) = dillo
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)

feedAnimal' :: (Weight, Animal) -> Animal
feedAnimal' (amount, Dillo Alive weight) = Dillo Alive (weight + amount)
feedAnimal' (amount, dillo@(Dillo Dead _)) = dillo
feedAnimal' (amount, Parrot sentence weight) = Parrot sentence (weight + amount)

-- untuplify :: ((Weight, Animal) -> Animal) -> (Weight -> (Animal -> Animal))
-- Haskell B. Curry
-- "currifzieren"
-- Moses Schönfinkel

schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
-- schönfinkeln f' = \ a -> \ b -> f' (a, b)
schönfinkeln f' a b = f' (a, b)

entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)
entschönfinkeln f (a, b) = f a b

{-
Eine geometrische Figur ("shape") ist:
- Kreis, hat Mittelpunkt und Radius - ODER -
- Quadrat, hat Ecke und Seitenlänger - ODER -
- eine Überlagerung zweier geometrischer Figuren

1. Repräsentation programmieren
2. Funktion schreiben, die für einen gegebenen Punkt sagt,
   ob dieser innerhalb der geometrischen Figur liegt oder nicht.
-}

data Position = Position Double Double

data Shape = Square { llCorner :: Position, sideLength :: Double }
--           | Circle

isIn (Position px py) (Square (Position cx cy) length) =
    px >= cx && py >= cy && px <= cx + length && py <= cy + length
-- isIn position Circle = ...

distance :: Position -> Position -> Double
distance (Position x1 y1) (Position x2 y2) =
    sqrt ((sqr (x1 - x2) + sqr (y1 - y2)))

sqr = \ x -> x * x