module Contract where

{-

- einfaches Beispiel
  "Ich bekomme 100€ am 24.12.2023."
  Zero-Coupon Bond
  Zero-Bond

- in "atomare Bestandteile" / "separate Ideen"
  - "Währung"
    "Ich bekomme 1€ jetzt."
  - "Vielfaches"
    "Ich bekomme 100€ jetzt."
  - "Später"

- Selbstbezüge / Kombinatoren

- nächstes Beispiel

Currency Swap:
Am 24.12.2023:
- Ich bekomme 100€.
- Ich bezahle 100GBP.
-}

type Amount = Double

data Date = MkDate String
  deriving (Show, Eq, Ord)

data Currency = EUR | GBP | YEN | USD
  deriving Show

{- Fehlversuch:
data Contract =
    ZeroCouponBond Date Amount Currency
-}

data Direction = Long | Short
  deriving Show

data Contract =
    One Currency -- "Ich bekomme 1€ jetzt."
  | Zero
  | Many Amount Contract
  | Later Date Contract
  | Both Contract Contract
--  | WithDirection Direction Contract
  | Inverse Contract -- vertauscht Rechte und Pflichten
  deriving Show

c1 = One EUR
-- Ich bekomme 100€ jetzt.
c2 = Many 100 (One EUR) 


-- Ich bekomme am 24.12.2023 100€.
zcb1 = Later (MkDate "2023-12-24") (Many 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency = 
    Later date (Many amount (One currency))

zcb1' = zeroCouponBond (MkDate "2023-12-24") 100 EUR

-- swap = Both (Later (MkDate "2023-12-24") (Many 100 (One EUR)))
--            (Later (MkDate "2023-12-24") (Many (-100) (One GBP)))

swap = Both (zeroCouponBond (MkDate "2023-12-24") 100 EUR)
            (Inverse (zeroCouponBond (MkDate "2023-12-24") 100 GBP))

-- Semantik