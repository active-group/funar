module Intro where

x :: Integer
x = 10

f :: Integer -> Integer
f n = n + 1

-- Ein Tier ist eins der folgenden:
-- - Gürteltier ODER
-- - Papagei

-- Ein Gürteltier hat folgende Eigenschaften:
-- - lebendig oder tot UND
-- - Gewicht

-- Ein Papagei hat folgende Eigenschaften:
-- - Satz
-- - Gewicht

data Liveness = Dead | Alive
  deriving Show -- toString

type Weight = Integer

data Animal =
    Dillo { dilloLiveness :: Liveness, dilloWeight :: Weight }
  | Parrot String Weight
  deriving Show

dillo1 :: Animal
dillo1 = Dillo Alive 10 -- lebendiges Gürteltier, 10kg schwer
parrot1 :: Animal
parrot1 = Parrot "Hallo!" 2  -- Papagei, grüßt nett, 2kg

-- Tier überfahren
-- Java:
{-
class Dillo {
    Liveness liveness;

    void runOver() {
        this.liveness = Liveness.Dead;
    }
}
-}
runOverAnimal :: Animal -> Animal
-- runOverAnimal (Dillo _liveness weight) = Dillo Dead weight
runOverAnimal dillo@(Dillo {}) = dillo { dilloLiveness = Dead }
runOverAnimal (Parrot _sentence weight) = Parrot "" weight

animals = [dillo1, parrot1]
deadAnimals = map runOverAnimal animals

-- Tier füttern
feedAnimal :: Weight -> (Animal -> Animal)
feedAnimal amount (Dillo liveness weight) = Dillo liveness (weight + amount)
feedAnimal amount (Parrot sentence weight) = Parrot sentence (weight + amount)


f1 = feedAnimal 1

{-
  Zero-Coupon Bond / Zero-Bond:
  Ich bekomme am 31.12.2020 100€.

  Beides:
  - Ich bekomme am 31.12.2020 100€.
  - Ich bezahle am 31.12.2020 100GBP.

  3 Ideen:
  - Währung
  - Betrag
  - Später

  FP: "Finde den Kombinator!"
  DDD: "Closure of operations": Operationen, die den gleichen Typ als Ausgabe haben wie als Eingabe


  0. Schritt: Modelliere so, wie der/die Domänenexperte/Domänenexpertin spricht
  1. Schritt: kleinste Bestandteile
  2. Schritt: bilde Kombinatoren
  3. Schritt: suche Kombinator T -> T -> T
  Schritt 3a: Assoziativgesetz (a + b) + c = a + (b + c)
              Both a (Both b c) == Both (Both a b) c
  Schritt 3b: neutrales Element a + 0 = 0 + a = a
              3a + 3b: Monoid
-}

data Date = Date String

type Amount = Double

data Currency = EUR | GBP

{-
data Contract =
    Zcb Date Amount Currency
  | Future
  | Himalaya
  | Call | Put
-}

data Contract =
      Zero
    | One Currency
    | Give Contract
    -- Kombinator
    | Multiple Amount Contract
    -- Jetzt Vertrag, später einen Vertrag abzuschließen
    | Later Date Contract
    | Both Contract Contract

-- Ich bekomme sofort 1€
c1 :: Contract
c1 = One EUR

-- Ich bekomme sofort 100€
c2 :: Contract
c2 = Multiple 100 c1 -- (One EUR)

c3 = Later (Date "2020-12-31") (Multiple 100 (One EUR))

c4 = Multiple 50 c3

zcb :: Date -> Amount -> Currency -> Contract
zcb date amount currency = Later date (Multiple amount (One currency))

c5 = zcb (Date "20200-12-31") 100 GBP 

c6 = Both c3 (give c5)

give (Give c) = c -- Gleichung
give c = Give c

data Payment = Payment Date Amount Currency

-- payments :: Contract -> [Payment]

step :: Contract -> Date -> ([Payment], Contract)
step Zero date = ([], Zero)
step (One currency) date =
    ([Payment date 1 currency], Zero)
step (Multiple amount contract) date = undefined
step (Later laterDate contract) date = undefined
step (Both contract1 contract2) date =
    let (payments1, contract1Rest) = step contract1 date
        (payments2, contract2Rest) = step contract2 date
    in (payments1 ++ payments2, Both contract1Rest contract2Rest)
step (Give contract) date = undefined