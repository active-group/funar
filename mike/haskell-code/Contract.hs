module Contract where

{-
Modellierungsprozeß:

- einfaches Beispiel
  Zero-Coupon Bond / Zero-Bond
  "Ich bekomme am 24.12.2023 100€."
-}

data Date = MkDate String
  deriving (Show, Eq, Ord)

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

data Contract =
      ZeroCouponBond Date Amount Currency 
    | Call
    | Put  
    | Future
    | Swap
    deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond (MkDate "2023-12-24") 100 EUR