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