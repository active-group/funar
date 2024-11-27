module Contract where

import Prelude hiding (and, reverse)

{-
- einfaches Beispiel
  Zero-Bond / zero-coupon bond
  "Ich bekomme am 24.12.2024 100€."
- Beispiel in "atomare Bestandteile" zerlegen
  1. Währung: "Ich bekomme 1€ jetzt."
  2. Betrag: "Ich bekomme 100€ jetzt."
  3. Später
- ... dabei Selbstbezüge eingeführt

- Currency-Swap:
  "Ich bekomme Weihnachten 100€ und ich zahle Weihnachten $100."
-}

data Date = MkDate String
  deriving (Show, Eq, Ord)

type Amount = Double

data Currency = EUR | CHF | USD | YEN | GBP
  deriving Show

christmas :: Date
christmas = MkDate "2024-12-24"

{-
data Contract =
    ZeroCouponBond Date Amount Currency
    deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond christmas 100 EUR
-}

data Direction = Long | Short
  deriving Show

data Contract =
    One Currency
  | Value Amount Contract
  | Later Date Contract
  | Reverse Contract
  | And Contract Contract 
  | Zero
  deriving Show

value :: Amount -> Contract -> Contract
value _ Zero = Zero
value amount contract = Value amount contract

and :: Contract -> Contract -> Contract
and c1 Zero = c1
and Zero c2 = c2
and c1 c2 = And c1 c2

reverse :: Contract -> Contract
reverse Zero = Zero
reverse c = Reverse c

instance Semigroup Contract where
    (<>) = and

instance Monoid Contract where
    mempty = Zero

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 :: Contract
c2 = Value 100 (One EUR)

-- "Ich bekomme 2000€ jetzt."
c3 :: Contract
c3 = Value 20 c2

zcb1 :: Contract
zcb1 = Later christmas (Value 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency = Later date (Value amount (One currency))

zcb1' :: Contract
zcb1' = zeroCouponBond christmas 100 EUR

-- "Ich zahle 100GBP jetzt."
c4 = Reverse (Value 100 (One GBP))

-- "Ich bekomme 100GBP jetzt."
-- c5 = Flow Long (Value 100 (One GBP))

-- "Ich bekomme 100GBP."
c6 = Reverse c4

-- "Ich bekomme Weihnachten 100€ und ich zahle Weihnachten $100."
c7 = And zcb1 (Later christmas (Reverse (Value 100 (One USD))))
c7' = Later christmas (And (Value 100 (One EUR)) (Reverse (Value 100 (One USD))))

fxSwap :: Date -> Amount -> Currency -> Amount -> Currency -> Contract
fxSwap date amount1 currency1 amount2 currency2 =
    And (zeroCouponBond date amount1 currency1)
        (Reverse (zeroCouponBond date amount2 currency2))

data Payment = MkPayment Direction Date Amount Currency
  deriving Show

-- Bedeutung / Denotation

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
  MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment Long date amount currency) = MkPayment Short date amount currency
invertPayment (MkPayment Short date amount currency) = MkPayment Long date amount currency

-- Zahlungen bis Datum + Residualvertrag
denotation :: Contract -> Date -> ([Payment], Contract)
denotation (One currency) now = ([MkPayment Long now 1 currency], Zero)
denotation (Value amount contract) now =
  let (payments, residualContract) = denotation contract now
   in (map (scalePayment amount) payments, value amount residualContract)
denotation c@(Later date contract) now = -- Alias-Pattern
  if now >= date
    then denotation contract now
    else ([], c)
denotation (Reverse contract) now =
  let (payments, residualContract) = denotation contract now
   in (map invertPayment payments, reverse residualContract)
denotation (And contract1 contract2) now =
  denotation contract1 now <> denotation contract2 now
denotation Zero now = mempty

-- denotation ist ein Monoiden-Homomorphismus

c8 :: Contract
c8 = Value 100 (Later christmas (One EUR))

-- >>> denotation c8 (MkDate "2024-11-27")
-- ([],Later (MkDate "2024-12-24") (One EUR))
