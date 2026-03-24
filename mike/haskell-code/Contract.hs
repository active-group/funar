module Contract where

{-
- einfaches Beispiel:
  zero-coupon bond / Zero-Bond
  "Ich bekomme am 24.12.2026 100€."

- Beispiel zerlegen in "atomare Bestandteile" / Bausteine
  - Währung: "Ich bekomme 1€ jetzt."
  - Betrag: "Ich bekomme 100€ jetzt."
  - Später

-}

data Date = MkDate String
  deriving (Eq, Ord, Show)

xmas :: Date
xmas = MkDate "2026-12-24"

data Currency = EUR | USD | CHF | YEN
  deriving Show

type Amount = Double

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Call
  | Put
  | FxSwap
  | Everest

zcb1 :: Contract
zcb1 = ZeroCouponBond xmas 100 EUR
-}

data Contract =
    One Currency
  | Many Amount Contract
  | Later Date Contract
  deriving Show

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 :: Contract
c2 = Many 100 (One EUR)

-- "Ich bekomme 100€ am 24.12.2026."
zcb1 :: Contract
zcb1 = Later xmas (Many 100 (One EUR))