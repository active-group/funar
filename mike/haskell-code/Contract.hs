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

-- Semantik ... Zahlungen bis zu Datum
semantics :: Contract -> Date -> [Payment]