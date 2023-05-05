module Contract where

{-

- einfaches Beispiel
  "Ich bekomme 100€ am 24.12.2023."
  Zero-Coupon Bond
  Zero-Bond

-}

type Amount = Double

data Date = MkDate String
  deriving (Show, Eq, Ord)

data Currency = EUR | GBP | YEN | USD
  deriving Show
  
data Contract =
    ZeroCouponBond Date Amount Currency