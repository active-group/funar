module Contract where

{-

1. einfaches Beispiel

Zero-Bond / Zero-Coupon Bond
"Ich bekomme am 24.12.2022 100€."

2. in "atomare Bestandteile" / Ideen zerlegen
- Währung ("Ich bekomme 1€ jetzt.")
- Vielfaches
- Später

-}

data Date = Date String deriving (Eq, Ord, Show)

christmas = Date "2022-12-24"

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Future
  | Call 
  | Put
    deriving Show

-- "Ich bekomme am 24.12.2022 100€"
zcb1 = ZeroCouponBond christmas 100 EUR

-}

data Contract =
    -- "Ich bekomme 1€ jetzt."
    One Currency
    -- "Ich bekomme 100€ jetzt."
  | Multiple Amount Contract
  | Later Date Contract
  deriving Show

-- "Ich bekomme 100€ jetzt."
c1 = Multiple 100 (One EUR)
zcb1 = Later christmas (Multiple 100 (One EUR))