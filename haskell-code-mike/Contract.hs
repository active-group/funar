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

-- Zahlungen bis zu einem bestimmten Datum
runContract :: Contract -> Date -> ([Payment], Contract) 
-- --------------------------------------------^^^^^^^^^  "Residualvertrag"
runContract (One currency) now = ([Payment Long now 1 currency], Zero)
runContract (Multiple amount contract') now =
    let (payments, residual) = runContract contract' now 
    in 

runContract Empty now = ([], Empty)

