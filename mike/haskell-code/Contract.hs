{-# LANGUAGE InstanceSigs #-}
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
    Zero
  | One Currency
  | WithMoney Amount Contract -- statt Currency
  | WithDate Date Contract
  | WithContract Contract Contract
  | Negate Contract
  deriving Show

instance Semigroup Contract where
    (<>) :: Contract -> Contract -> Contract
    (<>) = WithContract

instance Monoid Contract where
    mempty :: Contract
    mempty = Zero


-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 = WithMoney 100 (One EUR)

-- "Ich bekomme jetzt 50000€."
c3 = WithMoney 500 (WithMoney 100 (One EUR))

-- >>> mconcat [c1,c2,c3]
-- WithContract (One EUR) (WithContract (WithMoney 100.0 (One EUR)) (WithContract (WithMoney 500.0 (WithMoney 100.0 (One EUR))) Zero))

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
oneMonthLater date = date -- FIXME

monthly :: Date -> Date -> Contract -> Contract
monthly startDate endDate contract =
    if startDate > endDate
    then Zero
    else WithContract (WithDate startDate contract) 
                      (monthly (oneMonthLater startDate) endDate contract)

combineContracts :: [Contract] -> Contract
-- combineContracts list =
--    foldr WithContract Zero list
combineContracts = mconcat

-- Semantik

data Payment = MkPayment Date Direction Amount Currency
  deriving Show

data Direction = Long | Short
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
  MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment date Long amount currency) =
  MkPayment date Short amount currency
invertPayment (MkPayment date Short amount currency) =
  MkPayment date Long amount currency

-- Zahlungen bis zu Zeitpunkt, "heute"
-- ---> "Residualvertrag", was vom Vertrag übrigbleibt, nachdem die Zahlungen geleistet sind
meaning :: Contract -> Date -> ([Payment], Contract)
meaning Zero today = mempty
meaning (One currency) today = ([MkPayment today Long 1 currency], Zero)
meaning (WithMoney amount contract) today = 
    let (payments, residualContract) = meaning contract today
    in (map (scalePayment amount) payments, WithMoney amount residualContract)
meaning (Negate contract) today =
    let (payments, residualContract) = meaning contract today
    in (map invertPayment payments, Negate residualContract)
meaning (WithDate date contract) today =
    if today >= date 
    then meaning contract today
    else ([], WithDate date contract)
meaning (WithContract contract1 contract2) today =
    let (payments1, residualContract1) = meaning contract1 today
        (payments2, residualContract2) = meaning contract2 today
    in (payments1 ++ payments2, WithContract residualContract1 residualContract2)

-- >>> meaning (WithMoney 100 (One EUR)) xmas
-- ([MkPayment (MkDate "2025-12-24") Long 100.0 EUR],Zero)

{- Schablone:
meaning Zero today = (undefined, undefined)
meaning (One currency) today = (undefined, undefined)
meaning (WithMoney amount contract) today =
  let (payments, residualContract) = meaning contract today
   in (undefined, undefined)
meaning (Negate contract) today =
  let (payments, residualContract) = meaning contract today
   in (undefined, undefined)
meaning (WithDate date contract) today =
  let (payments, residualContract) = meaning contract today
   in (undefined, undefined)
meaning (WithContract contract1 contract2) today =
  let (payments1, residualContract1) = meaning contract1 today
      (payments2, residualContract2) = meaning contract2 today
   in (undefined, undefined)
-}

cfix = WithMoney 100 (WithContract (One EUR) (WithDate xmas (One EUR)))

-- >>> meaning cfix (MkDate "2025-05-14")
-- ([MkPayment (MkDate "2025-05-14") Long 100.0 EUR],WithMoney 100.0 (WithContract Zero (WithDate (MkDate "2025-12-24") (One EUR))))
