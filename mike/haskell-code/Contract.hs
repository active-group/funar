module Contract where

{-
Implizit: Vertrag zwischen zwei Parteien, eine davon "ich"

zero-coupon bond / Zero-Bond
"Ich bekomme am 24.12.2025 100€."
-}

newtype Date = MkDate String -- wie data
  deriving (Eq, Ord)

christmas = MkDate "2025-12-24"

data Contract 
    = ZeroCouponBond Date Amount Currency
