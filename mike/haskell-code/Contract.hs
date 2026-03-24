module Contract where

{-
- einfaches Beispiel:
  zero-coupon bond / Zero-Bond
  "Ich bekomme am 24.12.2026 100€."

- Beispiel zerlegen in "atomare Bestandteile" / Bausteine
  - Währung: "Ich bekomme 1€ jetzt."
  - Betrag: "Ich bekomme 100€ jetzt."
  - Später
  dabei: Selbstbezüge

- nächstes Beispiel:
  Currency Swap
  "Weihnachten bekomme ich 100€ und bezahle 100$."
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
    Zero
  | One Currency
  | Many Amount Contract
  | Later Date Contract
  | Put Contract
--  | Get Contract
  | And Contract Contract
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

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Many amount (One currency))

zcb1' :: Contract
zcb1' = zeroCouponBond xmas 100 EUR

-- "Ich bezahle jetzt 100€."
c3 :: Contract
c3 = Put (Many 100 (One EUR))

-- "Ich bekomme 100€."
c3' :: Contract
c3' = Put c3

-- "Ich bekomme jetzt 100€."
-- c4 :: Contract
-- c4 = Get (Many 100 (One EUR))

-- "Ich bekomme jetzt 100€."
-- c4' = Get c4

-- Semantik / "Bedeutung"

data Direction = Long | Short
  deriving Show

data Payment = MkPayment Date Direction Amount Currency
  deriving Show

-- Zahlungen bis zu einem Datum ("heute")
-- + "Residualvertrag"
meaning :: Contract -> Date -> ([Payment], Contract)

