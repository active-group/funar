module Contract where

-- 1. Einfaches Beispiel
-- Zero-Coupon Bond / Zero-Bond
-- "Ich bekomme 24.12.2022 100€."

-- 2. In "atomare Bestandteile" / "Ideen" zerlegen

-- "Währung": "Ich bekomme 1€ jetzt."
-- "Betrag" / "Vielfaches":
--    "Ich bekomme 100€ jetzt."
-- "Später"

-- 3. nächstes Beispiel
-- "Currency Swap"
-- "Weihnachten bekomme ich 100€ und bezahle $100."

newtype Date = Date String
  deriving (Show, Eq, Ord)

christmas = Date "2022-12-24"

type Amount = Double

data Currency = EUR | USD | GBP | YEN
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Call 
  | Put
  | FxSwap
  | Everest
    deriving Show

zcb1 = ZeroCouponBond christmas 100 EUR
-}

data Direction = Short | Long 
  deriving Show

data Contract =
    One Currency
  | Multiplier Amount Contract
  | Delayed Date Contract
  | Invert Contract
  | Combine Contract Contract -- Halbgruppe
  | Empty -- neutrales Element
  deriving Show

zcb1 = Delayed christmas (Multiplier 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Delayed date (Multiplier amount (One currency))

data Payment = MkPayment Direction Date Amount Currency
  deriving Show

invertPayment :: Payment -> Payment
invertPayment (MkPayment Short date amount currency) = 
    MkPayment Long date amount currency
invertPayment (MkPayment Long date amount currency) =
    MkPayment Short date amount currency

multiplyPayment :: Amount -> Payment -> Payment
multiplyPayment factor (MkPayment dir date amount currency) =
    MkPayment dir date (factor * amount) currency

-- alle Zahlungen bis zu diesem Datum
-- Rückgabe: Residualvertrag
semantics :: Contract -> Date -> ([Payment], Contract)
semantics Empty now = undefined
semantics (One currency) now = undefined
semantics (Multiplier amount contract) now = 
  let (payments, residualContract) = semantics contract now
  in (undefined, undefined)
semantics (Delayed date contract) now = undefined
semantics (Combine contract1 contract2) now = undefined
semantics (Invert contract) now = undefined