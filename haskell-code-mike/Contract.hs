module Contract where

-- einfaches Beispiel
-- Zero Coupon Bond / Zero-Bond
-- "Ich bekomme am 24.12.2022 100€."
-- implizit: Vertrag zwischen zwei Parteien, eine davon bin ich

-- Beispiel in Einzelteile / separaten Ideen:
-- - Währung: "1€ jetzt"
-- - Anzahl: "100€ jetzt"
-- - Später

-- Weitere Beispiele:
-- "Ich ZAHLE am 24.12.2022 100GBP."

data Date = Date String 
  deriving (Show, Eq, Ord)

type Amount = Double 

data Currency = EUR | USD | GBP
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Call 
  | Put 
  | Everest
  | Himalaya
  deriving Show

zcb1 = ZeroCouponBond (Date "2022-12-24") 100 EUR
-}

data Contract =
    One Currency
--  | OnePay Currency
  | Multiple Amount Contract
  | Later Date Contract
  | Negate Contract
  | Merge Contract Contract -- <- Halbgruppe!
  | Empty -- - neutrales Element
  deriving Show

-- Ich bekomme 100€ jetzt.
c1 = Multiple 100 (One EUR)
c2 = Multiple 500 c1

zcb1 = Later (Date "2022-12-24") (Multiple 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency = Later date (Multiple amount (One currency))

zcb2 = Negate (zeroCouponBond (Date "2022-12-24") 100 GBP)

data Direction = Long | Short
  deriving Show

data Payment = Payment Direction Date Amount Currency
  deriving Show

scalePayment factor (Payment direction date amount currency) =
    Payment direction date (factor * amount) currency

flipPayment (Payment Short date amount currency) = Payment Short date amount currency
flipPayment (Payment Long date amount currency) = Payment Long date amount currency

-- Merge x Empty == x

merge :: Contract -> Contract -> Contract
merge Empty contract = contract
merge contract Empty = contract
merge contract1 contract2 = Merge contract1 contract2

multiple :: Amount -> Contract -> Contract
multiple amount Empty = Empty
multiple amount contract = Multiple amount contract

negate :: Contract -> Contract
negate Empty = Empty
negate contract = contract

later date Empty = Empty
later date contract = Later date contract

-- >>> runContract zcb1 (Date "2022-12-31")

-- Zahlungen bis zu einem bestimmten Datum
runContract :: Contract -> Date -> ([Payment], Contract) 
-- --------------------------------------------^^^^^^^^^  "Residualvertrag"
runContract (One currency) now = ([Payment Long now 1 currency], Empty)
runContract (Multiple amount contract') now =
    let (payments, residual) = runContract contract' now 
    in (map (scalePayment amount) payments, Multiple amount residual)
runContract contract@(Later date contract') now =
    if now >= date 
    then runContract contract' now 
    else ([], contract)
runContract (Negate contract') now =
    let (payments, residual) = runContract contract' now 
    in (map flipPayment payments, Negate residual)    
runContract (Merge contract1 contract2) now =
    let (payments1, residual1) = runContract contract1 now 
        (payments2, residual2) = runContract contract2 now 
    in (payments1 ++ payments2, Merge residual1 residual2)
runContract Empty now = ([], Empty)

