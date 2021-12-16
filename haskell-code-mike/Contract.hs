module Contract where

{-
Einfaches Beispiel:
"Ich bekomme am 24.12.2021 100EUR."
Zero-Coupon Bond


-}

type Amount = Double 

data Currency = EUR | USD | GBP | YEN
  deriving Show

data Date = Date String 
  deriving (Show, Eq, Ord)

data Contract =
    ZeroCouponBond Date Amount Currency

zcb1 = ZeroCouponBond (Date "2021-12-24") 100 EUR