module Contract where

-- Zero-Coupon Bond / Zero-Bond
-- "Receive 100GBP on 29 Jan 2021"
type Amount = Double

data Date = Date String
  deriving (Show, Eq, Ord)

data Currency = GBP | EUR
  deriving Show

{-
data Contract
  = Zcb Amount Currency Date
  | Himalaya 
  | Screwdriver
-}

{-

 1. möglichst einfaches Domänenobjekt
 2. in möglichst kleine Bestandteile zerlegen
 3. finde Kombinatoren
-}

data Contract =
    One Currency
  | Multiple Amount Contract
  | Later Date Contract
  | Give Contract
  | Both Contract Contract
  | Zero

-- "100 GBP jetzt bekommen"
pounds100 = Multiple 100 (One GBP)

zcb1 = Later (Date "2021-01-29") pounds100

zcb :: Date -> Amount -> Currency -> Contract
zcb date amount currency = Later date (Multiple amount (One currency))

zcb2' = Give (zcb (Date "2020-08-31") 100 EUR)

swap1 = Both (zcb (Date "2020-12-31") 100 EUR)
             (Give (zcb (Date "2020-12-31") 100 GBP))

-- payments :: Contract -> [Payment]

data Direction = Long | Short
  deriving Show

data Payment = Payment Direction Date Amount Currency
  deriving Show

scalePayment factor (Payment direction date amount currency) =
    Payment direction date (factor * amount) currency

flipPayment (Payment Long date amount currency) = (Payment Short date amount currency)
flipPayment (Payment Short date amount currency) = (Payment Long date amount currency)

step :: Contract -> Date -> ([Payment], Contract)
step (One currency) date = ([Payment Long date 1 currency], Zero)
step (Multiple amount contract) date =
    let (payments, residualContract) = step contract date
    in (map (scalePayment amount) payments, Multiple amount residualContract)
step inputContract@(Later date' contract) date =
    if date >= date' 
    then step contract date
    else ([], inputContract)
step (Both contract1 contract2) date =
    let (payments1, residual1) = step contract1 date
        (payments2, residual2) = step contract2 date
    in (payments1 ++ payments2, Both residual1 residual2)
step (Give contract) date =
    let (payments, residualContract) = step contract date 
    in (map flipPayment payments, Give residualContract)
step Zero date = ([], Zero)

c1 = Both (zcb (Date "2020-12-31") 100 EUR)
          (zcb (Date "2021-12-31") 100 EUR)
