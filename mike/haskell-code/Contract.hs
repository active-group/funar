module Contract where

{-
- einfaches Beispiel
  Zero-Bond / zero-coupon bond
  "Ich bekomme am 24.12.2024 100€."

- Beispiel in "atomare Bestandteile" / "Ideen" aufteilen
  häufig: entlang der Attribute
-}

data Date = MkDate String
  deriving (Show, Eq, Ord)

type Amount = Double

data Currency = EUR | CHF | USD
  deriving Show

data Contract =
    ZeroCouponBond Date Amount Currency
  | Call 
  | Put
  | Everest
  | Himalaya