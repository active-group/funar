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

c6 = Multiple 100 (Combine (zeroCouponBond (MkDate "2023-1-24") 200 EUR)
                           (zeroCouponBond (MkDate "2024-1-24") 300 EUR))
-- >>> meaning c6 (MkDate "2023-31-01")
-- ([MkPayment (MkDate "2023-31-01") Long 20000.0 EUR],Multiple 100.0 (Combine (Multiple 200.0 Zero) (At (MkDate "2024-1-24") (Multiple 300.0 (One EUR)))))

-- "smart constructor"
multiple :: Amount -> Contract -> Contract
multiple _ Zero = Zero
multiple factor contract = Multiple factor contract

data Direction = Long | Short 
  deriving Show

invertDirection :: Direction -> Direction
invertDirection Long = Short
invertDirection Short = Long

data Payment = MkPayment Date Direction Amount Currency
  deriving Show 

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment date direction amount currency) =
    MkPayment date direction (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment date direction amount currency) =
    MkPayment date (invertDirection direction) amount currency


-- >>> (MkDate "2023-12-24") < (MkDate "2023-12-25")
-- True

-- alle Zahlungen bis zu einem bestimmten Zeitpunkt, "jetzt"
meaning :: Contract -> Date -> ([Payment], Contract)
meaning (One currency) now = ([MkPayment now Long 1 currency], Zero)
meaning (Multiple amount contract) now =
    let (payments, residualContract) = meaning contract now
    in (map (scalePayment amount) payments, Multiple amount residualContract)
meaning c@(At date contract) now =
    if now >= date
    then meaning contract now
    else ([], c)
meaning (Negative contract) now =
    let (payments, residualContract) = meaning contract now
    in (map invertPayment payments, Negative residualContract)
meaning (Combine contract1 contract2) now =
    let (payments1, residualContract1) = meaning contract1 now
        (payments2, residualContract2) = meaning contract2 now
    in (payments1 ++ payments2, Combine residualContract1 residualContract2)
meaning Zero now = ([], Zero)
