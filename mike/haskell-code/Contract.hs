module Contract where

{- vorweg: Vertrag zwischen zwei Parteien -}

{-
Vorgehensweise:

- einfaches Beispiel abfragen
  "Ich bekomme am 24.12.2025 100€."
  zero-coupon bond / Zero-Bond
-}

data Currency = EUR | USD | GBP | YEN
  deriving Show

type Amount = Double

data Date = MkDate String
  deriving (Show, Eq, Ord) -- sorgt dafür, daß mit <= etc. vergleichbar

xmas2025 = MkDate "2025-12-24"
today = MkDate "2025-10-07"

-- >>> xmas2025 > today
-- True

data Contract =
    ZeroCouponBond Date Amount Currency
    deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond xmas2025 100 EUR