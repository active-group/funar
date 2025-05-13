module Contract where

-- Finanzderivat:
-- (Bedingungen des) Vertrag zwischen zwei Parteien, "egozentrisch" aus Sicht der Bank

-- Domänenanalyse:

-- - einfaches Beispiel
--   Zero-Bond / zero-coupon bond
--   "Ich bekomme Weihnachten 100€."

data Date = MkDate String
  deriving (Show, Eq, Ord)

-- >>> :info Ord

data Contract =
    ZeroCouponBond Date Amount Currency
    deriving Show

