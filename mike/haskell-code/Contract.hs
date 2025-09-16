module Contract where

{-
Domänenmodellierung
- einfaches Beispiel

Zero-Bond / zero-coupon bond
"Ich bekomme am 24.12.2025 100€."

- zerlegen in "atomare Bestandteile" / "Ideen" ... z.B. entlang der Attribute
  - Währung: "Ich bekomme 1€ jetzt."
  - Betrag
  - Später

-}

data Currency = EUR | GBP | YEN | USD
  deriving Show

type Amount = Double

data Date = MkDate String
  deriving (Show, Eq, Ord)

xmas :: Date
xmas = MkDate "2025-12-24"

{-
data Contract =
    ZeroCouponBond Date Amount Currency
-}

data Contract =
    One Currency
    deriving Show

-- "Ich bekomme 1€ jetzt."
c1 = One EUR