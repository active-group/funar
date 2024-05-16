module Contract where

{-
1. einfaches Beispiel
   Zero-Bound / zero-coupon bond
   "Ich bekomme am 24.12.2024 100€."

2. einfache Beispiel in "atomare Bestandteile" / "Ideen" aufteilen

   - Währung 
   - Betrag
   - Später

   Currency Swap / FX Swap
   Am 24.12.2024:
     Ich bekomme 100€. -UND-
     Ich bezahle 100CHF.
-}

data Currency = EUR | CHF | USD | YEN
  deriving Show

data Date = MkDate String
  deriving (Show, Eq, Ord)

type Amount = Double

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Future
  | Get
  | Put
  deriving Show

zcb1 = ZeroCouponBond (MkDate "2024-12-24") 100 EUR
-}

data Contract =
     One Currency 
   | WithAmount Amount Contract
   | Later Date Contract
   | Give Contract  -- Zahlungsströme umdrehen
   | And Contract Contract  -- von der Bedeutung assoziativ => Halbgruppe
   | Zero
   deriving Show

instance Semigroup Contract where
    (<>) = And

instance Monoid Contract where
    mempty = Zero

-- Ich bekomme jetzt 1 EUR
c1 = One EUR

-- Ich bekomme jetzt 100 EUR
c100 = WithAmount 100 (One EUR)

zcb1 :: Contract
zcb1 = Later (MkDate "2024-12-24") (WithAmount 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (WithAmount amount (One currency))

zcb1' = zeroCouponBond (MkDate "2024-12-24") 100 EUR

-- Ich gebe Weihnachten 100CHF.
zcb2 = Give (zeroCouponBond (MkDate "2024-12-12") 100 CHF)

fx1 :: Contract
fx1 = And zcb1 zcb2

-- >>> fx1
-- And (Later (MkDate "2024-12-24") (WithAmount 100.0 (One EUR))) (Give (Later (MkDate "2024-12-12") (WithAmount 100.0 (One CHF))))

data Direction = Long | Short 
  deriving Show

data Payment = MkPayment Date Direction Amount Currency
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
  MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment date Long amount currency) = MkPayment date Short amount currency
invertPayment (MkPayment date Short amount currency) = MkPayment date Long amount currency

-- alle Zahlungen bis jetzt + Restvertrag
semantics :: Contract -> Date -> ([Payment], Contract)
semantics (One currency) now = ([MkPayment now Long 1 currency], Zero)
semantics (WithAmount amount contract) now =
  let (payments, residualContract) = semantics contract now
   in (map (scalePayment amount) payments, WithAmount amount residualContract)
semantics c@(Later date contract) now =
  if now >= date
    then semantics contract now
    else ([], c)
semantics (Give contract) now =
  let (payments, residualContract) = semantics contract now
   in (map invertPayment payments, Give residualContract)
semantics (And contract1 contract2) now =
  let (payments1, residualContract1) = semantics contract1 now
      (payments2, residualContract2) = semantics contract2 now
   in (payments1 ++ payments2, And residualContract1 residualContract2)
semantics Zero now = ([], Zero)

cc = And (One EUR) (WithAmount 100 (Later (MkDate "2024-12-24") (One EUR)))

-- >>> semantics cc (MkDate "2024-05-16")
-- ([MkPayment (MkDate "2024-05-16") Long 1.0 EUR],And Zero (WithAmount 100.0 (Later (MkDate "2024-12-24") (One EUR))))
