module Contract where

{-
- einfaches Beispiel
  Zero-Bond / zero-coupon bond
  "Ich bekomme am 24.12.2024 100€."
- Beispiel in "atomare Bestandteile" zerlegen
  1. Währung
  2. Betrag
  3. Später
-}

data Date = MkDate String
  deriving (Show, Eq, Ord)

type Amount = Double

data Currency = EUR | CHF | USD | YEN | GBP
  deriving Show

christmas :: Date
christmas = MkDate "2024-12-24"

{-
data Contract =
    ZeroCouponBond Date Amount Currency
    deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond christmas 100 EUR
-}

data Contract =
    