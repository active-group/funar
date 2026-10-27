module Contract where

{-
- einfaches Beispiel
  Zero-Coupon-Bond / Zero-Bond
  "Ich bekomme am 24.12.2026 100€."

-}

type Date = String  -- ISO-Format: YYYY-MM-DD

xmas :: Date
xmas = "2026-12-24"

isBefore :: Date -> Date -> Bool
isBefore s1 s2 = s1 <= s2

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

{}
data Contract =
    ZeroCouponBond Date Amount Currency
  | Put
  | Call 
  | Future
  | Everest


zcb1 :: Contract
zcb1 = ZeroCouponBond xmas 100 EUR