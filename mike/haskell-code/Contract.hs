module Contract where

{-
- einfaches Beispiel
  Zero-Bond / zero-coupon bond
  "Ich bekomme am 24.12.2024 100€."
-}

data Contract =
    ZeroCouponBond Date Amount Currency