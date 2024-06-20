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

data Contract =
    One Currency
  | Value Amount Contract
  | Later Date Contract

-- Ich bekomme 1€ jetzt.
c1 = One EUR

-- Ich bekomme 100€ jetzt.
c2 = Value 100 (One EUR)

-- Ich bekomme am 24.12.2024 100€.
c3 = Later (MkDate "2024-12-24") c2

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Value amount (One currency))
