module Contract where

import Prelude hiding (and, reverse)

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

multiplyPayment :: Amount -> Payment -> Payment
multiplyPayment factor (Payment dir date amount currency) =
    Payment dir date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (Payment Long date amount currency) =
    Payment Short date amount currency
invertPayment (Payment Short date amount currency) =
    Payment Long date amount currency

-- im Paper:
-- Later (Obs Amount) Contract
-- Obs: (zeitabhängige) Beobachtung

-- And Zero c =-= c
-- And c Zero =-= c
-- Multiple amount Zero =-= Zero
-- Reverse Zero =-= Zero

-- smart constructor
multiple _ Zero = Zero
multiple amount contract = Multiple amount contract

and :: Contract -> Contract -> Contract
and Zero c = c 
and c Zero = c
and c1 c2 = And c1 c2

-- >>> semantics (multiple 100 (One EUR)) (Date "2022-09-22")
-- ([Payment Long (Date "2022-09-22") 100.0 EUR],Zero)

-- alle Zahlungen bis zum Datum
semantics :: Contract -> Date -> ([Payment], Contract)
semantics Zero now =
    ([], Zero)
semantics (One currency) now =
    ([Payment Long now 1 currency], Zero)
semantics (Multiple amount contract) now =
    let (payments, contract') = semantics contract now
    in (map (multiplyPayment amount) payments, multiple amount contract')
semantics (Reverse contract) now =
    let (payments, contract') = semantics contract now
    in (map invertPayment payments, Reverse contract')
semantics (And contract1 contract2) now = 
    let (payments1, contract1') = semantics contract1 now
        (payments2, contract2') = semantics contract2 now
    in (payments1 ++ payments2, and contract1' contract2')   
semantics c@(Later date contract) now =
    if now >= date
    then -- Weihnachten
      semantics contract now
    else -- noch nicht Weihnachten
      ([], c)


