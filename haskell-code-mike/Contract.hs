module Contract where

{-
Einfaches Beispiel:
"Ich bekomme am 24.12.2021 100EUR."
Zero-Coupon Bond

Beispiel zerlegen in "atomare Bestandteile"
- Währung: Ich bekomme 1 EUR jetzt.
- Menge/Vielfaches: Ich bekomme 100 EUR jetzt.
- Später: Ich bekomme 100 EUR später.


-}

type Amount = Double 

data Currency = EUR | USD | GBP | YEN
  deriving Show

data Date = Date String 
  deriving (Show, Eq, Ord)

{-
data Contract =
    ZeroCouponBond Date Amount Currency

zcb1 = ZeroCouponBond (Date "2021-12-24") 100 EUR
-}

data Contract =
    One Currency
  | Multiple Amount Contract
  | Later Date Contract
  deriving Show

zcb1 = Later (Date "2021-12-24") (Multiple 100 (One EUR))

zeroCouponBond date amount currency =
    Later date (Multiple amount (One currency))

zcb1' = zeroCouponBond (Date "2021-12-24") 100 EUR
