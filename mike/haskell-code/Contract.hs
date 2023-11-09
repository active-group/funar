module Contract where

{-
- einfaches Beispiel
  Zero-Bond / Zero-Coupon Bond
  "Ich bekomme am 24.12.2023 100€."

- in atomare Bestandteile / Ideen zerlegen
  - Währung
  - Betrag
  - Später

- Selbstbezüge einbauen

- wieder von vorn:
  FX-Swap:
  "Am 24.12.2023 bekomme ich 100€ und zahle $100."

-}

data Date = Date String
  deriving (Eq, Ord, Show)

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Put
  | Call
  | Future
  deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond (Date "2023-12-24") 100 EUR 
-}

data Contract =
    One Currency
  | Amount Amount Contract -- <- Selbstbezug
  | Later Date Contract
  | Pay Contract
  | Combine Contract Contract
  -- Und vs. Oder
  | Empty 
  deriving Show

instance Semigroup Contract where
    (<>) = Combine

instance Monoid Contract where
    mempty = Empty

-- "Ich bekomme einen Euro jetzt."
c1 = One EUR

-- "Ich bekomme 10 Euro jetzt."
c2 = Amount 10 (One EUR)

-- "Ich zahle 10 Euro jetz."
c3 = Pay c2

zcb1 = Later (Date "2023-12-24") (Amount 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Amount amount (One currency))

zcb1' = zeroCouponBond (Date "2023-12-24") 100 EUR 

fxSwap = Combine zcb1 (Pay (zeroCouponBond (Date "2023-12-24") 100 GBP))

data Direction = Long | Short
  deriving Show

data Payment = Payment {
    paymentDate :: Date,
    paymentDirection :: Direction,
    paymentAmount :: Amount,
    paymentCurrency :: Currency
  }
  deriving Show

-- operationelle Semantik
semantics :: Contract -> Date -> ([Payment], Contract)
