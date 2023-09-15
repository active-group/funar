module Contract where

{-
- einfaches Beispiel
Zero-Bond  / Zero-Coupon Bond
"Ich bekomme am 24.12.2023 100€."

- Beispiel in "atomare Bestandteile" / "Ideen"

1. "Währung" - "Ich bekomme 1€ jetzt."
2. "Betrag" - "Ich bekomme 100€ jetzt."
3. "Später"

-}

data Date = Date String
  deriving (Eq, Ord, Show)

type Amount = Double

data Currency =
    EUR | USD | GBP | YEN
    deriving Show
{-
data Contract =
    ZeroCouponBond Date Amount Currency
    deriving Show

-- Ich bekomme am 23.12.2023 100€.
zcb1 = ZeroCouponBond (Date "2023-12-23") 100 EUR
-}

data Contract =
    One Currency
  | WithAmount Amount Contract -- Selbstbezug
  | DueDate Date Contract
  deriving Show

c1 = One EUR -- "Ich bekomme einen Euro jetzt."

c2 = WithAmount 100 (One EUR) -- "Ich bekomme 100€ jetzt."

zcb1 = DueDate (Date "2023-12-23") (WithAmount 100 (One EUR))