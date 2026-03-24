module Contract where

{-
- einfaches Beispiel:
  zero-coupon bond / Zero-Bond
  "Ich bekomme am 24.12.2026 100€."


-}

data Date = MkDate String
  deriving (Eq, Ord, Show)

xmas :: Date
xmas = MkDate "2026-12-24"

data Currency = EUR | USD | CHF | YEN
  deriving Show

type Amount = Double

data Contract =
    ZeroCouponBond Date Amount Currency

zcb1 :: Contract
zcb1 = ZeroCouponBond xmas 100 EUR
