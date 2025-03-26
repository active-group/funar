{-# LANGUAGE InstanceSigs #-}
module Contract where

{-
Implizit: Vertrag zwischen zwei Parteien, eine davon "ich"

1. einfaches Beispiel
zero-coupon bond / Zero-Bond
"Ich bekomme am 24.12.2025 100€."

2. Beispiel zerlegen in "atomare Bestandteile" / "Ideen"
   z.B. entlang der Attribute
   - Währung - "Ich bekomme jetzt 1EUR."
   - Betrag - "Ich bekomme 12€ jetzt."
   - "Später"

3. Selbstbezüge suchen/konstruieren

... und von vorne.

Currency-Swap / FxSwap:

Am 24.12.2025:
- Ich bekomme 100EUR und
  ich bezahle 100GBP.

-}

newtype Date = MkDate String -- wie data
  deriving (Eq, Ord, Show)

christmas = MkDate "2025-12-24"

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

{-
data Contract 
    = ZeroCouponBond Date Amount Currency
  deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond christmas 100 EUR
-}

data Direction = Long | Short
  deriving Show

data Contract
    = Zero
    | One Currency
    | Many Amount Contract
    | Later Date Contract
    | Inverse Contract
    | Combine Contract Contract
    deriving Show

instance Semigroup Contract where
    (<>) :: Contract -> Contract -> Contract
    (<>) = Combine

instance Monoid Contract where
    mempty = Zero

c1 = One EUR -- "Ich bekomme 1€ jetzt."


c2 = Many 12 (One EUR) -- "Ich bekomme 12€ jetzt."

c3 = Many 10 c2 -- "Ich bekomme 120€ jetzt."

c4 = Inverse (Inverse c3)

c5 = mconcat [c1,c2,c3,c4]

-- "Ich bekomme am 24.12.2025 100€."
zcb1' = Later christmas (Many 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Many amount (One currency))

zcb1 :: Contract
zcb1 = zeroCouponBond christmas 100 EUR

fxSwap1 :: Contract
fxSwap1 = Later christmas (Combine (Many 100 (One EUR))
                                   (Inverse (Many 100 (One GBP))))

fxSwap date longCurrency longAmount shortCurrency shortAmount =
    Combine (zeroCouponBond date longCurrency longAmount)
            (Inverse (zeroCouponBond date shortCurrency shortAmount))

-- >>> meaning (Many 50 (Later christmas (One EUR))) (MkDate "2025-03-26")

-- Semantik
data Payment = MkPayment Direction Date Amount Currency
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
  MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment Long date amount currency) =
    MkPayment Short date amount currency
invertPayment (MkPayment Short date amount currency) =
    MkPayment Long date amount currency

-- Zahlungen aus dem Vertrag bis heute
-- ----> "Residualvertrag"
meaning :: Contract -> Date -> ([Payment], Contract)
meaning Zero today = ([], Zero)
meaning (One currency) today =
    ([MkPayment Long today 1 currency], Zero)
meaning (Many amount contract) today =
    let (payments, residual) = meaning contract today
    in (map (scalePayment amount) payments, residual)
meaning c@(Later date contract) today =
    if today >= date
    then meaning contract today
    else ([], c)
meaning (Inverse contract) today =
    let (payments, residual) = meaning contract today
    in undefined
meaning (Combine contract1 contract2) today =
    let (payments1, residual1) = meaning contract1 today
        (payments2, residual2) = meaning contract2 today
    in (payments1 ++ payments2, Combine residual1 residual2)

today = MkDate "2025-03-26"

-- >>> meaning c2 today
-- ([MkPayment Long (MkDate "2025-03-26") 12 EUR], Zero)

-- >>> meaning (One EUR) today
-- ([MkPayment Long (MkDate "2025-03-26") 1 EUR], Zero)
-- >>> c2
