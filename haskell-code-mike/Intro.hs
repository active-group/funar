{-# LANGUAGE FlexibleInstances #-}

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

f1 = feedAnimal 1

o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)

-- f2 = o f1 f1
f2 = f1 `o` f1 
f2' = f1 . f1
 

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

{-
Eine Liste ist eins der folgenden:

- leere Liste
- Cons-Liste aus erstem Element und Rest-Liste
-}
{-
data List a =
    Empty
  | Cons a (List a)
  deriving Show

listSum :: List Integer -> Integer
listSum Empty = 0
listSum (first `Cons` rest) = first + (listSum rest)
-}

{-
leere Liste: []
Cons:   :  in Infix-Notation
-}

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first:rest) = first + (listSum rest)
-- listSum list = (head list) + (listSum (tail list))

-- (: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))

type List a = [a]
listMap :: (a -> b) -> List a -> List b
listMap _ [] = []
listMap f (first:rest) = 
    (f first) : (listMap f rest)


listFold :: b -> (a -> b -> b) -> [a] -> b
listFold forEmpty forCons [] = forEmpty
listFold forEmpty forCons (first:rest) = 
    first `forCons` (listFold forEmpty forCons rest)

data Map key value = Map [(key, value)]
  deriving Show

instance Functor (Map key) where
  fmap f (Map list) = Map (map (\ (key, value) -> (key, f value)) list)

data Optional a =
    Present a
  | Absent
  deriving Show

optionalMap :: (a -> b) -> Optional a -> Optional b
optionalMap f Absent = Absent
optionalMap f (Present x) = Present (f x)

instance Functor Optional where
  fmap = optionalMap

instance Applicative Optional where

instance Monad Optional where
  -- return :: a -> Optional a
  return = Present

  -- (>>=) :: Optional a -> (a -> Optional b) -> Optional b
  (Present result) >>= next = next result
  Absent >>= next = Absent

op1 :: Integer -> Optional Integer 
op1 = undefined

op2 :: Integer -> Optional Integer 
op2 = undefined 

op12 :: Optional Integer
op12 = do x <- op1 10
          y <- op2 20
          return (x+ y)

-- in der Kategorietheorie: Funktor
class Mappable f where
    universalMap :: (a -> b) -> f a -> f b

instance Mappable Optional where
    universalMap = optionalMap

instance Mappable [] where
    universalMap = listMap

{- Eingebaut:
data Maybe a =
    Just a
  | Nothing
-}

mapLookup :: Eq key => key -> Map key value -> Optional value
mapLookup key (Map []) = Absent
mapLookup key (Map ((key', value'):rest)) =
    if key == key'
    then Present value'
    else mapLookup key (Map rest) 

{-
--- "Interface"
class Eq a where
    (==) :: a -> a -> Bool

-- "Implementation"
instance Eq Bool where
    (==) True True = True
    (==) False False = True
    (==) _ _ = False

-}

instance Eq a => Eq (Optional a) where
    (==) (Present x) (Present y) = x == y 
    (==) Absent Absent = True 
    (==) _ _ = False


natsFrom :: Integer -> [Integer]
natsFrom n = n : natsFrom (n + 1)

strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n l = filter (\ x -> x `rem` n /= 0) l

sieve :: [Integer] -> [Integer]
sieve (first:rest) = first : sieve (strikeMultiples first rest)
sieve [] = []

primes = sieve (natsFrom 2)

-- Menge / Typ: m
-- Operation(en):
-- op :: m -> m -> m (Beispiel)
-- neutral :: m
-- Gesetze / Gleichungen:
-- op m x == op x m == x für alle x aus m
-- x + 0 == 0 + x == x

-- Halbgruppe:
-- Menge / Typ m
-- op :: m -> m -> m
-- op (op a b) c == op a (op b c)
-- Assoziativgesetz:
-- (a + b) + c == a + (b + c)

-- Halbgruppe + neutrales Element: Monoid

data State state a = State (state -> (a, state))

write :: state -> State state ()
write newState =
  State (\ _ -> ((), newState))

read :: State state state
read = State (\ state -> (state, state))

instance Functor (State state) where
  -- fmap :: (a -> b) -> (State state) a -> (State state) b
  fmap f (State t) =
    State (\ state -> 
             let (a, state') = t state
             in (f a, state'))

instance Applicative (State state) where

instance Monad (State state) where
  -- return :: a -> State state a
  return a = State (\ state -> (a, state))
  -- (>>=) :: State state a -> (a -> State state b) -> State state b
