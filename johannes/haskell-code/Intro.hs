{-# LANGUAGE InstanceSigs #-}
module Intro where

-- Signatur:
x :: Integer -- feste Typen: groß, Werte klein
x = 7

y :: Integer
y = 12 * x + 5

-- Eine Zahl verdoppeln
-- (: f (number -> number))
f :: Integer -> Integer
-- f = \ n -> n * 2 -- \ ist Lambda!
-- f 0 = 0
-- f 1 = 2
-- f 2 = 4
f n = n * 2

-- Ein Haustier ist eins der Folgenden:
-- - Katze -ODER-
-- - Hund -ODER-
-- - Schlange
data Pet = Cat | Dog | Snake
    deriving Show  -- denkt: toString()

-- Ist ein Haustier niedlich?
isCute :: Pet -> Bool
-- 1 Gleichung pro Fall (Racket: eine cond-Klausel pro Fall)
-- Pattern Matching: matchen auf best. Werte von Pet
-- Funktionen in Haskell sind _TOTAL_:
-- - zu jedem Input _muss_ es einen Output geben
isCute Cat = True
isCute Dog = True
isCute Snake = False

-- g n = 2 * n

-- Ein Gürteltier hat folgende Eigenschaften:
-- - Gewicht
-- - Lebenszustand
data Liveness = Alive | Dead
    deriving Show

type Weight = Integer

-- MkDillo == make-dillo : Daten-Konstruktor
-- dilloLiveness == dillo-alive? : Selektor, Accessor
-- dilloWeight == dillo-weight
-- >>> :t MkDillo
-- MkDillo :: Liveness -> (Weight -> Dillo)
-- Geschw. Klammern sind Spezialsyntax für Records
-- data Dillo = MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
--     deriving Show

dillo1 :: Animal
dillo1 = MkDillo Alive 10

dillo2 :: Animal
dillo2 = MkDillo { dilloLiveness = Dead, dilloWeight = 5 }

-- runOverDillo :: Dillo -> Dillo
-- weiß noch nicht, was ich hinschreiben soll? -> undefined
-- >>> runOverDillo dillo1
-- MkDillo {dilloLiveness = Dead, dilloWeight = 10}
-- >>> runOverDillo dillo2
-- MkDillo {dilloLiveness = Dead, dilloWeight = 5}

-- runOverDillo dillo = MkDillo Dead (dilloWeight dillo)
-- runOverDillo (MkDillo Alive weight) = MkDillo Dead weight
-- runOverDillo (MkDillo Dead weight) = MkDillo Dead weight
-- runOverDillo dillo = dillo

-- functional update syntax
-- runOverDillo dillo = dillo { dilloLiveness = Dead }

-- _ : "don't care"
-- runOverDillo (MkDillo { dilloLiveness = _, dilloWeight = weight }) =
-- runOverDillo (MkDillo { dilloWeight = weight }) =
--     MkDillo Dead weight

-- data Dillo = MkDillo Liveness Weight

type Sentence = String

-- gemischte Daten:
-- Ein Tier ist eins der Folgenden:
-- - Dillo
-- - Papagei
data Animal =
    -- MkDillo Dillo -> Problem mit nicht-totalen Funktionen umgehen
    MkDillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | MkParrot { parrotSentence :: Sentence, parrotWeight :: Weight }
--   | MkParrot Sentence Weight
  deriving Show

-- Tiere überfahren
runOverAnimal :: Animal -> Animal
runOverAnimal (MkDillo _ weight) = MkDillo Dead weight
runOverAnimal (MkParrot _ weight) = MkParrot "" weight

type Amount = Integer

-- Tiere füttern
feedAnimal :: Animal -> Amount -> Animal
-- Alias-Pattern
feedAnimal dillo@(MkDillo liveness weight) amount =
    -- Fallunterscheidung
    case liveness of
        Dead -> dillo
        Alive -> MkDillo Alive (weight + amount)
feedAnimal parrot amount = parrot { parrotWeight = parrotWeight parrot + amount }

type PhValue = Integer

data HairType = Curly | Straight

-- Duschprodukt ist eins der Folgenden:
-- - Seife (hat pH-Wert)
-- - Shampoo (hat Haartyp)
-- - Duschgel (_IMMER_ 50% Seife, 50% Shampoo)
data ShowerProduct
    = MkSoap PhValue
    | MkShampoo HairType
    | MkShowerGel -- HairType PhValue
    | Mixture Percentage ShowerProduct ShowerProduct

type Percentage = Integer

-- Seifenanteil berechnen
soapPercentage :: ShowerProduct -> Percentage
soapPercentage (MkSoap _) = 100
soapPercentage (MkShampoo _) = 0
soapPercentage MkShowerGel = 50
soapPercentage (Mixture ratio p1 p2) =
    undefined
    -- (soapPercentage p1) / 100 + soapPercentage p2

-- 1) Datenanalyse + Datendefinition
-- 2) Funktion, die den Seifenanteil eines Duschprodukts berechnet
-- 3) - weitere Ausprägung von Duschprodukten:
--      Mixtur aus zwei Duschprodukten, zu beliebigen
--      Anteilen gemischt

-- Division?
-- Selbstbezug?
