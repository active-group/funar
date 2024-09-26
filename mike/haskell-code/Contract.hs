module Contract where
{-
- einfaches Beispiel
  Zero-Bond / Zero-Coupon Bond
  "Ich bekomme am 24.12.2024 100€."
- aufspalten in "atomare Bestandteile" / "Ideen"
  - Datum
  - Betrag
  - Währung
-}
data Date = Date String
  deriving (Eq, Ord, Show)

type Amount = Double

data Currency = EUR | YEN | GBP | CHF
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Call
  | Put 
  | Himalaja
  | Butterblume
-}

data Contract =
    -- Ich bekomme einen Euro. Jetzt.
    One Currency
  | Amount Amount Contract
  | At Date Contract
  deriving Show

c1 :: Contract
-- "Ich bekomme jetzt einen Euro."
c1 = One EUR

-- Ich bekomme jetzt 100€.
c100 = Amount 100 (One EUR)

zcb1 = At (Date "2024-12-24") (Amount 100 (One EUR))