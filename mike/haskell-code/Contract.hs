module Contract where

{-

- einfaches Beispiel
  "Ich bekomme 100€ am 24.12.2023."
  Zero-Coupon Bond
  Zero-Bond

- in "atomare Bestandteile" / "separate Ideen"
  - "Währung"
    "Ich bekomme 1€ jetzt."
  - "Vielfaches"
    "Ich bekomme 100€ jetzt."
  - "Später"


-}

type Amount = Double

data Date = MkDate String
  deriving (Show, Eq, Ord)

data Currency = EUR | GBP | YEN | USD
  deriving Show

{- Fehlversuch:
data Contract =
    ZeroCouponBond Date Amount Currency
-}

data Contract =
    One Currency -- "Ich bekomme 1€ jetzt."
  | Many Amount Contract
  | Later Date Contract
  deriving Show

c1 = One EUR
-- Ich bekomme 100€ jetzt.
c2 = Many 100 (One EUR) 


-- Ich bekomme am 24.12.2023 100€.
zcb1 = Later (MkDate "2023-12-24") (Many 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency = 
    Later date (Many amount (One currency))

zcb1' = zeroCouponBond (MkDate "2023-12-24") 100 EUR