module Contract where

{-
Implizit: Vertrag zwischen zwei Parteien, eine davon "ich"

zero-coupon bond / Zero-Bond
"Ich bekomme am 24.12.2025 100€."
-}

newtype Date = MkDate String -- wie data
  deriving (Eq, Ord, Show)

christmas = MkDate "2025-12-24"

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

data Contract 
    = ZeroCouponBond Date Amount Currency
  deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond christmas 100 EUR