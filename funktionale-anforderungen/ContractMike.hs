module Contract where

data Date = Date String
  deriving (Eq, Ord, Show)

data Currency = EUR | GBP
  deriving (Show, Eq)

-- Suche kleine Verträge!

data Contract = -- Zcb Date Double Currency -- ???
    Zero
  | One Currency
  | Scale Double Contract -- Currency
  | When Date Contract
  | And Contract Contract
  | Give Contract
  deriving (Eq, Show)

-- Zero-Coupon Bond
-- "in der Zukunft"
-- 100 Stück
-- Währung
zcb :: Date -> Double -> Currency -> Contract
zcb date amount currency = -- Zcb
  When date (Scale amount (One currency))

date = Date

-- "Zahle mir 100 GBP am 1.10.2019"
c1 = zcb (date "2019-10-01") 100 GBP
c2 = zcb (date "2020-10-01") 100 EUR
c3 = And c1 c2