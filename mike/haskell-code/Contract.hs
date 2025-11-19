{-# LANGUAGE InstanceSigs #-}
module Contract where

import Prelude hiding (and)

{-
Identifizieren, worum es geht - besondere "Dinge".

1. einfaches Beispiel
   Zero-Coupon Bond / Zero-Bond
   "Ich bekomme Weihnachten 100€."

2. Beispiel in "atomare" Bestandteile zerlegen
   - "später"
   - "Vielfaches"
   - "Währung"

   Einfachster Vertrag, in dem nur die Währung eine Rolle spielt.
   "Ich bekomme jetzt 1€."

   + "Vielfaches"
   "Ich bekomme jetzt 100€."

3. Nächstes Beispiel, wh.
   Currency-Swap
   Weihnachten bekomme ich 100€ UND zahle 90GBP.

fehlt: "und", "rein und raus"
-}

data Date = MkDate String -- YYYY-MM-DD
 deriving (Show, Eq, Ord)

xmas :: Date
xmas = MkDate "2025-12-24"
easter :: Date
easter = MkDate "2025-04-20"

type Amount = Double

data Currency = EUR | GBP | YEN | USD
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Call
  | Put
  | Future

zcb1 :: Contract
zcb1 = ZeroCouponBond xmas 100 EUR
-}

data Direction = Long | Short
  deriving Show

data Contract =
    Zero
  | One Currency
  | Multiple Amount Contract -- Selbstbezug ==> Kombinator
  | Later Date Contract
--  | Flow Direction Contract
  | Shorten Contract
  | And Contract Contract
  deriving Show

-- "smart constructor"
and :: Contract -> Contract -> Contract
and Zero c = c
and c Zero = c
and c1 c2 = And c1 c2

multiple :: Amount -> Contract -> Contract
multiple _ Zero = Zero
multiple factor c = Multiple factor c

instance Semigroup Contract where
  (<>) :: Contract -> Contract -> Contract
  (<>) = and

instance Monoid Contract where
  mempty = Zero

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 :: Contract
c2 = Multiple 100 (One EUR)

-- "Ich bekomme 5000€ jetzt."
c3 :: Contract
c3 = Multiple 50 c2

-- "Ich zahle 5000€ jetzt."
c4 :: Contract
c4 = Shorten c3

{-
-- "Ich bekomme 5000€ jetzt"
c5 :: Contract
c5 = Flow Short c4

-- "Ich bekomme 5000€ jetzt."
c6 :: Contract
c6 = Flow Long c3
-}

zcb1 :: Contract
zcb1 = Later xmas (Multiple 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
  Later date (Multiple amount (One currency))

zcb1' :: Contract
zcb1' = zeroCouponBond xmas 100 EUR

fxSwap1 :: Contract
fxSwap1 = Later xmas (And (Multiple 100 (One EUR))
                          (Shorten (Multiple 90 (One EUR))))

fxSwap1' = And (zeroCouponBond xmas 100 EUR)
               (Shorten (zeroCouponBond xmas 90 GBP))

data Payment = MkPayment Date Direction Amount Currency
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
  MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment date Long amount currency) =
  MkPayment date Short amount currency
invertPayment (MkPayment date Short amount currency) =
  MkPayment date Long amount currency

-- Bedeutung eines Vertrags
-- "Zahlungen bis zum Datum (heute)" + Residualvertrag
meaning :: Contract -> Date -> ([Payment], Contract)
meaning Zero today = ([], Zero)
meaning (One currency) today = ([MkPayment today Long 1 currency], Zero)
meaning (Multiple amount contract) today =
  let (payments, residualContract) = meaning contract today
   in (map (scalePayment amount) payments, multiple amount residualContract)
meaning (Shorten contract) today =
  let (payments, residualContract) = meaning contract today
   in (map invertPayment payments, Shorten residualContract)
meaning (Later date contract) today =
  if today >= date
    then meaning contract today
    else ([], Later date contract)
meaning (And contract1 contract2) today =
  let (payments1, residualContract1) = meaning contract1 today
      (payments2, residualContract2) = meaning contract2 today
   in (payments1 ++ payments2, and residualContract1 residualContract2)

-- >>> meaning c9 (MkDate "2025-12-01")
-- ([MkPayment (MkDate "2025-12-01") Long 100.0 EUR],Multiple 100.0 (Later (MkDate "2025-12-24") (One EUR)))

-- >>> meaning c9 xmas
-- ([MkPayment (MkDate "2025-12-24") Long 100.0 EUR,MkPayment (MkDate "2025-12-24") Long 100.0 EUR],Zero)


c9 :: Contract
c9 = Multiple 100 (And (One EUR) (Later xmas (One EUR)))
