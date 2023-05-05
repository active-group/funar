module Intro where

-- Prelude: Haskell-Standardbibliothek
import Prelude hiding (Semigroup, Monoid)

-- Groß-/Kleinschreibung signifikant
x :: Integer
x = 43

y :: Integer
y = x * 3

f :: Integer -> Integer
-- >>> f 17 
-- 34
f = \ x -> x * 2

f' :: Integer -> Integer
f' x = x * 2

g :: Integer -> Integer -> Integer
-- g x y =  (x + y) * 2
-- >>> g 5 7
-- 24
g = \ x -> \ y -> (x + y) * 2

-- Haustier ist eins der folgenden:
-- - Hund -ODER-
-- - Katze -ODER-
-- - Schlange

-- neuer Datentyp:
data Pet = -- Typ
    Dog -- Konstruktor (hier 0stellig)
  | Cat
  | Snake
  deriving (Eq, Show)

{-
instance Eq Pet where
    --- (==) :: Pet -> Pet -> Bool
    (==) Cat Cat = True
    (==) Dog Dog = True
    (==) Snake Snake = True
    (==) _ _ = False
-}

-- Abseitsregel: Bei mehrzeiligen Konstrukten müssen die
-- Folgezeilen gegenüber der erste Zeile eingerückt sein.

-- Intuition: Kleinbuchstaben = Variablen, Großbuchstaben = Konstanten

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- >>> isCute Dog
-- True
-- >>> isCute Snake
-- False
-- isCute pet =
--    case pet of
--        Dog -> True
--        Cat -> True
--        Snake -> False
isCute Dog = True
isCute Cat = True
isCute Snake = False

-- Ein Gürteltier hat folgende Eigenschaften:
-- - tot oder lebendig
-- - Gewicht
data Liveness = Dead | Alive
  deriving Show

-- Typsynonym
type Weight = Integer

{-
data Dillo = MkDillo { liveness :: Liveness, weight :: Weight }
  deriving Show

-- lebendiges Gürteltier, 10kg
dillo1 :: Dillo
dillo1 = MkDillo { liveness = Alive, weight = 10 }

dillo2 :: Dillo
dillo2 = MkDillo Dead 8 -- Abkürzung

-- Gürteltier überfahren
runOverDillo :: Dillo -> Dillo
-- >>> runOverDillo dillo1
-- MkDillo {liveness = Dead, weight = 10}

-- >>> runOverDillo dillo2
-- MkDillo {liveness = Dead, weight = 8}

-- runOverDillo dillo = MkDillo { liveness = Dead, weight = weight dillo }
-- runOverDillo dillo = MkDillo Dead (weight dillo)
-- runOverDillo (MkDillo { liveness = l, weight = w}) = MkDillo Dead w
-- runOverDillo (MkDillo _ w) = MkDillo Dead w -- _: don't care
-- runOverDillo (MkDillo { weight = w}) = MkDillo Dead w
-- runOverDillo dillo = dillo { liveness = Dead } -- "functional update", Kopie bis auf ...

-- runOverDillo dillo@(MkDillo l w) = -- Alias-Pattern
--    case l of
--        Dead -> dillo
--        Alive -> MkDillo Dead w

runOverDillo dillo@(MkDillo Dead _) = dillo
runOverDillo (MkDillo Alive w) = MkDillo Dead w

data Parrot = MkParrot String Weight
-}

-- Ein Tier ist eins der folgenden:
-- - Gürteltier
-- - Papagei

data Animal =
    MkDillo { liveness :: Liveness, weight :: Weight}
  | MkParrot String Weight
  deriving Show
-- typisch: gemischte Daten, jeder Fall zusammengesetzte Daten
-- algebraischer Datentyp

dillo1 :: Animal
dillo1 = MkDillo { liveness = Alive, weight = 10}
dillo2 :: Animal
dillo2 = MkDillo Dead 8

parrot1 :: Animal
parrot1 = MkParrot "Welcome!" 1
parrot2 :: Animal
parrot2 = MkParrot "Goodbye!" 2

runOverAnimal :: Animal -> Animal
-- >>> runOverAnimal dillo1
-- MkDillo {liveness = Dead, weight = 10}

-- >>> runOverAnimal parrot1
-- MkParrot "" 1
runOverAnimal (MkDillo _ w) = MkDillo Dead w
runOverAnimal (MkParrot _ w) = MkParrot "" w

-- In Haskell gibt es nur einstellige Funktionen.

-- >>> feedAnimal dillo1 5
-- MkDillo {liveness = Alive, weight = 15}
feedAnimal :: Animal -> Weight -> Animal
feedAnimal dillo@(MkDillo liveness weight) amount = 
    case liveness of
        Dead -> dillo
        Alive -> MkDillo liveness (weight+amount)
feedAnimal (MkParrot sentence weight) amount =
    MkParrot sentence (weight+amount)

-- >>> (swap feedAnimal) 5 dillo1
-- MkDillo {liveness = Alive, weight = 15}

-- eingebaut als flip
-- swap :: (Animal -> Weight -> Animal) -> (Weight -> Animal -> Animal)
swap :: (a -> b -> c) -> (b -> a -> c) -- Typvariablen
-- swap f = \ b -> \ a -> f a b
swap f b a = f a b

feedAnimal' :: (Animal, Weight) -> Animal
-- >>> feedAnimal'(dillo1, 5)
-- MkDillo {liveness = Alive, weight = 15}
feedAnimal' (dillo@(MkDillo liveness weight), amount) =
  case liveness of
    Dead -> dillo
    Alive -> MkDillo liveness (weight + amount)
feedAnimal' ((MkParrot sentence weight), amount) =
  MkParrot sentence (weight + amount)

-- eingebaut als curry
schönfinkeln :: ((a, b) -> c) -> (a -> b -> c)
schönfinkeln f = \ a -> \b -> f (a, b)

-- eingebaut uncurry
entschönfinkeln :: (a -> b -> c) -> ((a, b) -> c)
entschönfinkeln f = \ (a, b) -> f a b

-- Eine geometrische Figur ("Shape") ist eins der folgenden:
-- - ein Kreis
-- - ein Quadrat
-- - eine Überlagerung zweier geometrischer Figuren

-- Dafür Datendefinition und Code, außerdem:
-- eine Funktion die für eine geometrische Figur und einen Punkt
-- ermittelt, ob der Punkt innerhalb der außerhalb der geometrischen
-- Figur ist.

-- Eine Liste ist eins der folgenden:
-- - die leere Liste
-- - eine Cons-Liste aus erstem Element und Rest-Liste
{-
data ListOf a =
      Empty 
    | Cons Integer (ListOf a)
    deriving Show
-}

-- In Haskell:
-- leere Liste: []
-- Cons-Liste:  first : rest

-- Summe aller Listenelemente berechnen
listSum :: [Integer] -> Integer
listSum [] = 0
listSum (first : rest) = first + (listSum rest)

type List a = [a]

listMap :: (a -> b) -> List a -> List b
listMap f [] = []
listMap f (x : xs) = f x : listMap f xs

listFold :: a -> (b -> a -> a) -> [b] -> a 
listFold n f [] = n
listFold n f (x  :                xs) = -- f x (listFold n f xs)
              x `f` (listFold n f xs)

-- Funktionskomposition
o :: (b -> c) -> (a -> b) -> (a -> c)
o f g = \ a -> f (g a)
-- eingebaut als .

data Optional a =
    Result a
  | Null
  deriving Show

-- >>> Result 15 == Result 15
-- True

-- >>> Result 15 == Null
-- False

-- >>> Result 15 == Result 17
-- False

instance Eq a => Eq (Optional a) where
  (==) Null Null = True
  (==) (Result a) (Result a') = a == a'
  (==) Null (Result a) = False
  (==) (Result a) Null = False

-- data Maybe a = Nothing | Just a

-- Position eines Elements in einer Liste finden
listIndex :: Eq a => a -> [a] -> Optional Integer
-- >>> listIndex 3 [5, 7, 2, 1, 3, 9, 3]
-- Result 4

-- >>> listIndex 3 [5, 7, 2]
-- Null

listIndex y [] = Null
listIndex y (x:xs) =
    if y == x
    then Result 0 
    else 
      optionalMap (\ index -> index + 1) (listIndex y xs)
{-
        case listIndex y xs of
            Null -> Null
            Result index -> Result (index+1)
-}

optionalMap :: (a -> b) -> Optional a -> Optional b
{-
optionalMap f Null = Null
optionalMap f (Result a) = Result (f a)
-}
optionalMap f o =
  case o of
    Null -> Null
    Result a -> Result (f a)

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

instance Functor Optional where
  fmap = optionalMap

instance Applicative Optional where
  pure = Result
  (<*>) 

instance Monad Optional where
  (>>=) Null next = Null
  (>>=) (Result a) next = next a
  return = Result

-- Eq a: Constraint

-- >>> :info Eq
-- type Eq :: * -> Constraint
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool

-- Typklasse: eher ein Interface
-- Implementierung einer Typklasse: instance

-- >>> :info Show
-- type Show :: * -> Constraint
-- class Show a where
--   show :: a -> String

-- >>> :info Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a

-- >>> :info Num
-- type Num :: * -> Constraint
-- class Num a where
--   (+) :: a -> a -> a
--   (-) :: a -> a -> a
--   (*) :: a -> a -> a
--   negate :: a -> a
--   abs :: a -> a
--   signum :: a -> a
--   fromInteger :: Integer -> a

-- deriving funktioniert für Show, Eq, Ord

-- ... neutrales Element ...

-- Menge + Operationen + Gleichungen

-- Zahlen, *, a * 1 = 1 * a = a

-- Menge M (in der Programmierung: Typ)
-- op :: M -> M -> M   (Kombinator, binäre Operation)
-- (Beispiele: *, +, Duschgel, Shape, overlay, beside, above)
-- op a (op b c) == op (op a b) c
-- a * (b * c) == (a * b) * c    Assoziativität
-- Halbgruppe / Semigroup

class Semigroup m where
  -- op a (op b c) == op (op a b) c
  op :: m -> m -> m

-- >>> op [1,2,3] [4,5,6]
-- [1,2,3,4,5,6]

-- >>> op [1,2,3] (op [4,5,6] [7,8,9])
-- [1,2,3,4,5,6,7,8,9]
-- >>> op (op [1,2,3] [4,5,6]) [7,8,9]
-- [1,2,3,4,5,6,7,8,9]

-- Kommutativität:
-- op a b == op b a

instance Semigroup [a] where
  op list1 list2 = list1 ++ list2

-- Halbgruppe + neutrales Element:
-- neutral :: M
-- op neutral a == op a neutral = a
-- Monoid

-- "m muß eine "
class Semigroup m => Monoid m where
  neutral :: m

-- >>> op [1,2,3] neutral
-- [1,2,3]

instance Monoid [a] where
  neutral = []

-- Übung:
-- Instanzen für Monoid (a, b), Monoid (Optional a)

instance (Semigroup a, Semigroup b) => Semigroup (a, b) where
  op (a, b) (a', b') = (op a a', op b b')

instance (Monoid a, Monoid b) => Monoid (a, b) where
  neutral = (neutral, neutral)

instance Semigroup a => Semigroup (Optional a) where
  op Null Null = Null
  op (Result a) Null = Result a
  op Null (Result a) = Result a
  op (Result a) (Result a') = Result (op a a')

instance Semigroup a => Monoid (Optional a) where
  neutral = Null

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a  -- das gleiche wie return
--   (<*>) :: f (a -> b) -> f a -> f b

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

--   fmap ::       (a ->   b) -> f a -> f b
--   (<*>) ::    f (a ->   b) -> f a -> f b
-- (flip (>>=)) :: (a -> m b) -> m a -> m b
