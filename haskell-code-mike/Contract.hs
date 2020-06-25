module Contract where

-- Zero-Coupon Bond
-- "Ich bekomme am 24.12.2020 100EUR"
-- "Ich bekomme am 30.12.2021 100GBP"

-- 1. Atomare Bestandteile:
-- "Später"
-- Währung
-- Stückzahl

-- 2. Kombinatoren suchen

-- 3. wiederholen mit mehr Beispielen

-- Swap: Ich bekomme 100EUR und zahle 100GBP.

data Date = Date String
    deriving (Eq, Ord, Show)

type Amount = Double

data Currency = EUR | GBP
    deriving (Eq, Show)

-- Idee: Vertrag ist immer "jetzt"
data Contract =
--    Zcb Date Amount Currency
    Zero
  | One Currency
  | Multiple Amount Contract
  -- Later d c: Zum Zeitpunkt d werde ich den Vertrag c abschließen
  | Later Date Contract
  | Give Contract -- dreht Richtung aller Zahlungen um
  -- And :: Contract -> Contract -> Contract
  -- Halbgruppe - check
  | And Contract Contract
  deriving (Eq, Show)

zcb1 = Later (Date "2020-12-24") (Multiple 100 (One EUR))
zcb2 = Later (Date "2021-12-30") (Multiple 100 (One GBP))

zcb date amount currency = Later date (Multiple amount (One currency))

contract1 = And zcb1 zcb2

-- Semantik

-- Für einen Vertrag die Zahlungen ausspucken

data Direction = Long | Short
  deriving Show

data Payment = Payment Direction Amount Currency
  deriving Show

step :: Contract -> Date -> ([Payment], Contract) 
                                    --  "Residualvertrag"
step Zero date = undefined
step (One currency) date =
    ([Payment Long 1 currency], Zero)
step (Multiple amount contract) date = undefined
step (Later amount contract) date = undefined
step (Give contract) date = undefined
step (And contract1 contract2) date = undefined