module Contract where

{-
1. einfaches Beispiel
Zero-Bond / Zero Coupon Bond
"Ich bekomme am 24.12.2022 100€."

-}

data Date = Date String
 deriving (Eq, Ord, Show)

date1 = Date "2022-12-24"

data Currency = EUR | GBP | YEN | USD
  deriving Show

type Amount = Double

data Contract =
    ZeroCouponBond Date Amount Currency
  | Future
  | Call 
  | Put