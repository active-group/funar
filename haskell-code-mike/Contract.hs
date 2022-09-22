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

data Direction = Long | Short
  deriving Show

data Contract =
    -- "Ich bekomme 1€ jetzt"
    One Currency
    -- "Ich bekomme 100€ jetzt."    
  | Multiple Amount Contract
    -- "Ich bekomme 100€ am 24.12.2022"
  | Later Date Contract
  -- | Flux Direction Contract
  -- besser
  | Reverse Contract
  | And Contract Contract -- Semigroup?
  | Zero -- neutrales Element
  deriving Show

c1 = One EUR
c2 = Multiple 100 (One EUR)
c3 = Later (Date "2022-12-24") c2

-- c3' = Flux Long c3

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Multiple amount (One currency))

c4 = zeroCouponBond (Date "2022-12-24") 100 USD

-- currency swap
c5 = And c3 (Reverse c4)

-- Semantik

data Payment = Payment Direction Date Amount Currency
  deriving Show

-- im Paper:
-- Later (Obs Amount) Contract
-- Obs: (zeitabhängige) Beobachtung

-- alle Zahlungen bis zum Datum
semantics :: Contract -> Date -> [Payment]