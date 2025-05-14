module Contract where

-- Finanzderivat:
-- (Bedingungen des) Vertrag zwischen zwei Parteien, "egozentrisch" aus Sicht der Bank

-- Domänenanalyse:

-- - einfaches Beispiel
--   Zero-Bond / zero-coupon bond
--   "Ich bekomme Weihnachten 100€."

-- - Beispiel in "atomare Bestandteile" / "Bauteile" zerlegt

-- - dabei Kombinatoren konstruiert

-- wiederholen

-- Currency Swap:
-- Am 24.12.2025:
--    Ich bekomme 100€ und
--    ich zahle $100.

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
  | WithDate Date Contract
  | WithContract Contract Contract
  | Negate Contract
  deriving Show

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 = WithMoney 100 (One EUR)

-- "Ich bekomme jetzt 50000€."
c3 = WithMoney 500 (WithMoney 100 (One EUR))

zcb1 :: Contract
zcb1 = WithDate (MkDate "2025-12-24") (WithMoney 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    WithDate date (WithMoney amount (One currency))

zcb1' = zeroCouponBond (MkDate "2025-12-24") 100 EUR

xmas = MkDate "2025-12-24"

fxSwap1 :: Contract
fxSwap1 = WithDate xmas (WithContract (WithMoney 100 (One EUR))
                                      (Negate (WithMoney 100 (One USD))))

fxSwap1' = WithContract (zeroCouponBond xmas 100 EUR)
                        (Negate (zeroCouponBond xmas 100 USD))
        
oneMonthLater :: Date -> Date
oneMonthLater date = date

monthly :: Date -> Contract -> Contract
monthly startDate contract =
    WithContract (WithDate startDate contract) (monthly (oneMonthLater startDate) contract)