module Intro where

-- Typsignatur
x :: Integer
x = 5

y :: Integer
y = x + 12

-- >>> f 17
f a = a + x + y

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
runOverDillo dillo = dillo { dilloLiveness = Dead }