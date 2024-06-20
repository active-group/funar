{-# LANGUAGE InstanceSigs #-}
module Contract where

{-
- einfaches Beispiel
  Zero-Bond / zero-coupon bond
  "Ich bekomme am 24.12.2024 100€."

- Beispiel in "atomare Bestandteile" / "Ideen" aufteilen
  häufig: entlang der Attribute

  - Währung: "Ich bekomme jetzt 1€."
  - Betrag: "Ich bekomme jetzt 100€."
  - Später: "Ich bekomme am 24.12.2024 ..."

  ... dabei nach Selbstbezügen suchen

- wiederholen

  Currency Swap:
  Ich bekomme am 24.12.2024 100€. -UND-
  Ich bezahle am 24.12.2024 100CHF.
-}

data Date = MkDate String
  deriving (Show, Eq, Ord)

type Amount = Double

data Currency = EUR | CHF | USD
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Call 
  | Put
  | Everest
  | Himalaya
-}

data Direction = Long | Short
  deriving Show

data Contract =
    Zero
  | One Currency
  | Value Amount Contract
  | Later Date Contract
  | Combine Contract Contract
  | FlipDirection Contract
  deriving Show

instance Semigroup Contract where
    (<>) :: Contract -> Contract -> Contract
    (<>) = Combine

instance Monoid Contract where
    mempty :: Contract
    mempty = Zero

combine :: Contract -> Contract -> Contract
combine c1 Zero = c1
combine Zero c2 = c2
combine c1 c2 = Combine c1 c2

-- Ich bekomme 1€ jetzt.
c1 = One EUR

-- Ich bekomme 100€ jetzt.
c2 = Value 100 (One EUR)

-- Ich bekomme am 24.12.2024 100€.
c3 = Later (MkDate "2024-12-24") c2

-- Ich bezahle jetzt 1€.
c4 = FlipDirection c1

c5 = FlipDirection c4

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Value amount (One currency))

fxSwap1 =
    Combine c3
       (FlipDirection (zeroCouponBond (MkDate "2024-12-24") 100 CHF))

data Payment = MkPayment Date Direction Amount Currency
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment date direction amount currency) =
    MkPayment date direction (factor * amount) currency

flipPayment (MkPayment date Long amount currency) =
    MkPayment date Short amount currency
flipPayment (MkPayment date Short amount currency) =
  MkPayment date Long amount currency

-- Semantik ... Zahlungen bis zu Datum -> Residualvertrag
semantics :: Contract -> Date -> ([Payment], Contract)
semantics Zero now = ([], Zero)
semantics (One currency) now = 
    ([MkPayment now Long 1 currency], Zero)
semantics (Value factor contract) now =
    let (payments, residualContract) = semantics contract now
    in (map (scalePayment factor) payments, Value factor residualContract)
semantics c@(Later date contract) now =
    if now >= date
    then semantics contract now
    else ([], c)
semantics (Combine contract1 contract2) now =
    let (payments1, residualContract1) = semantics contract1 now
        (payments2, residualContract2) = semantics contract2 now
    in (payments1 ++ payments2, combine residualContract1 residualContract2)
semantics (FlipDirection contract) now =
    let (payments, residualContract) = semantics contract now
    in (map flipPayment payments, FlipDirection residualContract)

-- let (payments, residualContract) = semantics ...
-- in ...

c6 :: Contract
c6 = Value 100 (Combine (One EUR) (Later (MkDate "2024-12-24") (One EUR)))

-- >>> semantics c6 (MkDate "2024-07-01")
-- ([MkPayment (MkDate "2024-07-01") Long 100.0 EUR],Value 100.0 (Combine Zero (Later (MkDate "2024-12-24") (One EUR))))
