module Contract where

{-
1. einfaches Beispiel
Zero-Bond / Zero Coupon Bond
"Ich bekomme am 24.12.2022 100€."

2. Beispiel in "atomare" Teile / Ideen zerlegen
- Währung
- Vielfaches
- Später

3. Entferne Redundanzen zu Gunsten von Selbstreferenzen

4. Repeat
-}

data Date = Date String
 deriving (Eq, Ord, Show)

date1 = Date "2022-12-24"

data Currency = EUR | GBP | YEN | USD
  deriving Show

type Amount = Double

-- Scheißspiel:
{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Future
  | Call 
  | Put
-}

data Contract =
    -- "Ich bekomme 1€ jetzt"
    One Currency
    -- "Ich bekomme 100€ jetzt."    
  | Multiple Amount Contract
    -- "Ich bekomme 100€ am 24.12.2022"
  | Later Date Contract
  | 

c1 = One EUR
c2 = Multiple 100 (One EUR)
c3 = Later (Date "2022-12-24") c2

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Multiple amount (One currency))