module Contract where

{-
- einfaches Beispiel
  Zero-Bond / zero-coupon bond
  "Ich bekomme am 24.12.2024 100€."
- Beispiel in "atomare Bestandteile" zerlegen
  1. Währung: "Ich bekomme 1€ jetzt."
  2. Betrag: "Ich bekomme 100€ jetzt."
  3. Später
- ... dabei Selbstbezüge eingeführt

- Currency-Swap:
  "Ich bekomme Weihnachten 100€ und ich zahle Weihnachten $100."
-}

data Date = MkDate String
  deriving (Show, Eq, Ord)

type Amount = Double

data Currency = EUR | CHF | USD | YEN | GBP
  deriving Show

christmas :: Date
christmas = MkDate "2024-12-24"

{-
data Contract =
    ZeroCouponBond Date Amount Currency
    deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond christmas 100 EUR
-}

data Direction = Long | Short
  deriving Show

data Contract =
    One Currency
  | Value Amount Contract
  | Later Date Contract
  deriving Show


-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 :: Contract
c2 = Value 100 (One EUR)

-- "Ich bekomme 2000€ jetzt."
c3 :: Contract
c3 = Value 20 c2

zcb1 :: Contract
zcb1 = Later christmas (Value 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency = Later date (Value amount (One currency))

zcb1' :: Contract
zcb1' = zeroCouponBond christmas 100 EUR