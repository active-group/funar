module Contract where

{-
- einfaches Beispiel
  Zero-Coupon-Bond / Zero-Bond
  "Ich bekomme am 24.12.2026 100€."

-}

data Date = MkDate String  -- ISO-Format: YYYY-MM-DD
  deriving Show

isBefore :: Date -> Date -> Bool
isBefore (MkDate s1) (MkDate s2) = s1 <= s2

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

data Contract =
    ZeroCouponBond Date Amount Currency

xmas :: Date
xmas = MkDate "2026-12-24"

zcb1 :: Contract
zcb1 = ZeroCouponBond xmas 100 EUR