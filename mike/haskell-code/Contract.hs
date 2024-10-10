{-# LANGUAGE InstanceSigs #-}
module Contract where

{-
Vertrag implizit zwischen Bank und Kund:in

1. einfaches Beispiel
Zero-Bond / zero-coupon bond
"Ich bekomme 100€ am 24.12.2024."

2. Beispiel in "atomare Bestandteile"/Ideen zerlegen
- Betrag
- Währung
- Später
... mit Selbstbezügen / Kombinatoren

3. wiederholen, mit größeren Beispiel
Currency Swap:
- Ich bekomme am 24.12.2024 100€ und
- ich bezahle am selben Tag 100USD.
-}

type Amount = Double

data Currency = EUR | GBP | YEN | USD
  deriving Show

data Date = MkDate String
  deriving (Eq, Ord, Show)

{-
-- blöd:
data Contract =
    ZeroCouponBond Amount Currency Date
  | Call 
  | Put 
  | Himalaya 
  | Butterblume

zcb1 = ZeroCouponBond 100 EUR (MkDate "2024-12-24")
-}

data Contract =
    -- Währung
    -- "Ich bekomme 1€ jetzt."
    One Currency
    -- Betrag
    -- "Ich bekomme 5€ jetzt."
  | Times Amount Contract -- Selbstbezug
    -- später
  | AtDueDate Date Contract
  | And Contract Contract
  | Negate Contract -- dreht alle Zahlungsrichtungen um
  | Zero
  deriving Show

instance Semigroup Contract where
    (<>) :: Contract -> Contract -> Contract
    (<>) = and'

instance Monoid Contract where
    mempty = Zero


-- Ich bekomme 1€ jetzt
c1 = One EUR

c5 = Times 5 (One EUR)

--- "Ich bekomme am 24.12.2024 100€."
zcb1 = AtDueDate (MkDate "2024-12-24") (Times 100 (One EUR))

-- "Ich bezahle am 24.12.2024 100€."
zcb1r = Negate zcb1


zcb1rr = Negate zcb1r

c2 = AtDueDate (MkDate "2024-02-01") (AtDueDate (MkDate "2024-02-02") (Times 5 (One EUR)))

zeroCouponBond :: Amount -> Currency -> Date -> Contract
zeroCouponBond amount currency date = AtDueDate date (Times amount (One currency))

zcb1' = zeroCouponBond 100 EUR (MkDate "2024-12-24")

data Direction = Long | Short
  deriving Show

data Payment = MkPayment Direction Date Amount Currency
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
  MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment Long date amount currency) = MkPayment Short date amount currency
invertPayment (MkPayment Short date amount currency) = MkPayment Long date amount currency


-- Denotation
-- Datum: "heute"/"jetzt", Zahlungen bis heute
-- Output: "Residualvertrag"
denotation :: Contract -> Date -> ([Payment], Contract)
denotation (One currency) now = ([MkPayment Long now 1 currency], mempty)
denotation (Times amount contract) now =
  let (payments, residualContract) = denotation contract now
   in (map (scalePayment amount) payments, Times amount residualContract)
denotation c@(AtDueDate date contract) now =
  if now >= date
    then denotation contract now
    else ([], c)
denotation (Negate contract) now =
  let (payments, residualContract) = denotation contract now
   in (map invertPayment payments, Negate residualContract)
denotation (And contract1 contract2) now =
    {-
  let (payments1, residualContract1) = denotation contract1 now
      (payments2, residualContract2) = denotation contract2 now
   in (payments1, residualContract1) <> (payments2, residualContract2)
   -}
   denotation contract1 now <> denotation contract2 now

denotation Zero now = mempty

-- smart constructor
and' :: Contract -> Contract -> Contract
and' Zero c = c
and' c Zero = c
and' c1 c2 = And c1 c2

negate :: Contract -> Contract
negate Zero = Zero
negate c = Negate c

cc = Times 100 (And (One EUR) (AtDueDate (MkDate "2024-12-24") (One EUR)))

-- >>> denotation cc (MkDate "2024-10-10")
-- ([MkPayment Long (MkDate "2024-10-10") 100.0 EUR],Times 100.0 (AtDueDate (MkDate "2024-12-24") (One EUR)))

-- >>> and' Zero Zero
-- Zero
