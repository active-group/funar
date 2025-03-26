module Contract where

{-
Implizit: Vertrag zwischen zwei Parteien, eine davon "ich"

1. einfaches Beispiel
zero-coupon bond / Zero-Bond
"Ich bekomme am 24.12.2025 100€."

2. Beispiel zerlegen in "atomare Bestandteile" / "Ideen"
   z.B. entlang der Attribute
   - Währung - "Ich bekomme jetzt 1EUR."
   - Betrag - "Ich bekomme 12€ jetzt."
   - "Später"

-}

newtype Date = MkDate String -- wie data
  deriving (Eq, Ord, Show)

christmas = MkDate "2025-12-24"

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

{-
data Contract 
    = ZeroCouponBond Date Amount Currency
  deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond christmas 100 EUR
-}

data Contract
    = One Currency
    | Many Amount Contract
    | Later Date Contract
    deriving Show

c1 = One EUR -- "Ich bekomme 1€ jetzt."


c2 = Many 12 (One EUR) -- "Ich bekomme 12€ jetzt."

c3 = Many 10 c2 -- "Ich bekomme 120€ jetzt."

-- "Ich bekomme am 24.12.2025 100€."
zcb1' = Later christmas (Many 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Many amount (One currency))

zcb1 :: Contract
zcb1 = zeroCouponBond christmas 100 EUR
