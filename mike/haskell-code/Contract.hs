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