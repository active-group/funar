module Contract where

-- Finanzderivat:
-- (Bedingungen des) Vertrag zwischen zwei Parteien, "egozentrisch" aus Sicht der Bank

-- Domänenanalyse:

-- - einfaches Beispiel
--   Zero-Bond / zero-coupon bond
--   "Ich bekomme Weihnachten 100€."

-- - Beispiel in "atomare Bestandteile" / "Bauteile"

-- 3 Ideen:
---  1. Währung
---  2. Betrag
---  3. Später

newtype Date = MkDate String -- newtype: 1 Fall, 1 Attribut, wie data
  deriving (Show, Eq, Ord)

-- >>> :info Ord
-- type Ord :: * -> Constraint
-- class Eq a => Ord a where
--   compare :: a -> a -> Ordering
--   (<) :: a -> a -> Bool
--   (<=) :: a -> a -> Bool
--   (>) :: a -> a -> Bool
--   (>=) :: a -> a -> Bool
--   max :: a -> a -> a
--   min :: a -> a -> a

type Amount = Double

data Currency =
    EUR | GBP | USD | YEN | CHF
    deriving Show

{-
data Contract =
   ZeroCouponBond Date Amount Currency
   deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond (MkDate "2025-12-24") 100 EUR
-}

data Contract =
    One Currency
  | WithMoney Amount Contract -- statt Currency
  deriving Show

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."