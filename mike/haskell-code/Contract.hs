module Contract where

{-
Domänenmodellierung
- einfaches Beispiel

Zero-Bond / zero-coupon bond
"Ich bekomme am 24.12.2025 100€."

- zerlegen in "atomare Bestandteile" / "Ideen" ... z.B. entlang der Attribute
  - Währung: "Ich bekomme 1€ jetzt."
  - Betrag: "Ich bekomme 100€ jetzt."
  - Später

- dabei Kombinatoren (Konstruktoren mit Selbstbezügen)

Currency Swap / Fx Swap:
Weihnachten bekomme ich 100€ und bezahle 100GBP.

-}

data Currency = EUR | GBP | YEN | USD
  deriving Show

type Amount = Double

data Date = MkDate String
  deriving (Show, Eq, Ord)

xmas :: Date
xmas = MkDate "2025-12-24"

{-
data Contract =
    ZeroCouponBond Date Amount Currency
-}

data Direction = Long | Short
  deriving Show

data Contract =
    -- | eine Einheit der Währung bekommen
    One Currency
  | WithAmount Amount Contract
  | WithDate Date Contract 
  | WithChangedDirection Contract -- Zahlungsrichtungen umdrehen
  | Two Contract Contract
  | Zero
  deriving Show

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 :: Contract
c2 = WithAmount 100 (One EUR)

-- "Ich zahle 100€ jetzt."
c3 :: Contract
c3 = WithChangedDirection c2

-- "Ich bekomme 100€ jetzt."
-- c4 = WithDirection Long c2

-- "Ich bekomme 100€ jetzt."
c5 :: Contract
c5 = WithChangedDirection c3

-- "Ich bekomme Weihnachten 100€."
zcb1 :: Contract
zcb1 = WithDate xmas (WithAmount 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency = WithDate date (WithAmount amount (One currency))

-- Currency-Swap
fxSwap :: Date -> Amount -> Currency -> Amount -> Currency -> Contract
fxSwap date myAmount myCurrency theirAmount theirCurrency =
    Two (zeroCouponBond date myAmount myCurrency)
        (WithChangedDirection (zeroCouponBond date theirAmount theirCurrency))

data Payment = MkPayment Date Direction Amount Currency
  deriving Show

two :: Contract -> Contract -> Contract
two Zero Zero = Zero
two contract1 contract2 = Two contract1 contract2

-- operationelle Semantik
-- Zahlungen bis zu einem bestimmten Zeitpunkt + Residualvertrag
meaning :: Contract -> Date -> ([Payment], Contract)
meaning Zero today = ([], Zero)
meaning (One currency) today = ([MkPayment today Long 1 currency], Zero)
meaning (WithAmount amount contract) today =
  let (payments, residualContract) = meaning contract today
   in (map (scalePayment amount) payments, WithAmount amount residualContract)
meaning (WithChangedDirection contract) today =
  let (payments, residualContract) = meaning contract today
   in (map invertPayment payments, WithChangedDirection residualContract)
meaning c@(WithDate date contract) today =
  if today >= date
    then meaning contract today
    else ([], c) -- WithDate date contract
meaning (Two contract1 contract2) today =
  let (payments1, residualContract1) = meaning contract1 today
      (payments2, residualContract2) = meaning contract2 today
  in (payments1 ++ payments2, Two residualContract1 residualContract2)

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
  MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment date Long amount currency) =
  MkPayment date Short amount currency
invertPayment (MkPayment date Short amount currency) =
  MkPayment date Long amount currency

-- >>> meaning c6 (MkDate "2025-10-01")
-- ([MkPayment (MkDate "2025-10-01") Long 100.0 EUR],WithAmount 100.0 (Two Zero (WithDate (MkDate "2025-12-24") (One EUR))))
-- >>> meaning c6 xmas
-- ([MkPayment (MkDate "2025-12-24") Long 100.0 EUR,MkPayment (MkDate "2025-12-24") Long 100.0 EUR],WithAmount 100.0 (Two Zero Zero))
c6 = WithAmount 100 (Two (One EUR) (WithDate xmas (One EUR)))
