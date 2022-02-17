module Contract where

-- einfaches Beispiel
-- Zero Coupon Bond / Zero-Bond
-- "Ich bekomme am 24.12.2022 100€."
-- implizit: Vertrag zwischen zwei Parteien, eine davon bin ich

data Contract =
    ZeroCouponBond Date Amount Currency