module Contract where

{-
1. einfaches Beispiel
Zero-Bond / Zero Coupon Bond
"Ich bekomme am 24.12.2022 100€."

-}

data Date = MkDate String deriving (Eq, Ord, Show)

type Amount = Float

data Currency = EUR | GBP | USD | YEN
  deriving Show

data Contract =
    ZeroCouponBond Date Amount Currency
  | Future Date -- ...
  | Put
  | Call
    deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond (MkDate "2022-12-24") 100 EUR