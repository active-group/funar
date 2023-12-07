module Contract where

{-
Modellierungsprozeß:

- einfaches Beispiel
  Zero-Coupon Bond / Zero-Bond
  "Ich bekomme am 24.12.2023 100€."

- einfaches Beispiel in "atomare Bestandteile"/"Ideen" zerlegen
  z.B. entlang der Attribute

  - "Währung": "Ich bekomme 1€ jetzt."
  - "Betrag": "Ich bekomme 100€ jetzt."

  - "Später": "Ich bekomme 100€ am 24.12.2023"

- Currency Swap:
  Am 24.2.2023:
  "Ich bekomme 100€ und ich zahle $100."
-}

data Date = MkDate String
  deriving (Show, Eq, Ord)

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

{-
data Contract =
      ZeroCouponBond Date Amount Currency 
    | Call
    | Put  
    | Future
    | Swap
    deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond (MkDate "2023-12-24") 100 EUR
-}

data Contract =
      One Currency
    | Multiple Amount Contract
    | At Date Contract
    | Negative Contract
    | Combine Contract Contract
    | Zero
    deriving Show

instance Semigroup Contract where
    (<>) = Combine

instance Monoid Contract where
    mempty = Zero

-- "Ich bekomme 1€ jetzt"
c1 = One EUR

-- "Ich bekomme 100€ jetzt"
c2 = Multiple 100 (One EUR)

c3 = Multiple 100.5 (One EUR)

zcb1 :: Contract
zcb1 = At (MkDate "2023-12-24") (Multiple 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    At date (Multiple amount (One currency))

zcb1' :: Contract
zcb1' = zeroCouponBond (MkDate "2023-12-24") 100 EUR


-- "Ich zahle 1€ jetzt."
c4 = Negative (One EUR)

-- "Ich bekomme 1€ jetzt."
c5 = Negative c4

swap1 :: Contract
swap1 = Combine (zeroCouponBond (MkDate "2023-12-24") 100 EUR)
                (Negative (zeroCouponBond (MkDate "2023-12-24") 100 USD))

data Direction = Long | Short 
  deriving Show

data Payment = MkPayment Date Direction Amount Currency
  deriving Show 

-- alle Zahlungen bis zu einem bestimmten Zeitpunkt, "jetzt"
meaning :: Contract -> Date -> ([Payment], Contract)
