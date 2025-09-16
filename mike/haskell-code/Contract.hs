module Contract where

{-
Domänenmodellierung
- einfaches Beispiel

Zero-Bond / zero-coupon bond
"Ich bekomme am 24.12.2025 100€."

- zerlegen in "atomare Bestandteile" / "Ideen" ... z.B. entlang der Attribute
  - Währung: "Ich bekomme 1€ jetzt."
  - Betrag: "Ich bekomme 100€ jetzt."
  - Später

- dabei Kombinatoren (Konstruktoren mit Selbstbezügen)

Currency Swap / Fx Swap:
Weihnachten bekomme ich 100€ und bezahle 100GBP.

-}

data Currency = EUR | GBP | YEN | USD
  deriving Show

type Amount = Double

data Date = MkDate String
  deriving (Show, Eq, Ord)

xmas :: Date
xmas = MkDate "2025-12-24"

{-
data Contract =
    ZeroCouponBond Date Amount Currency
-}

data Direction = Long | Short
  deriving Show

data Contract =
    One Currency
  | WithAmount Amount Contract
  | WithDate Date Contract 
  | WithChangedDirection Contract -- Zahlungsrichtungen umdrehen
  | Two Contract Contract
  | Zero
  deriving Show

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 :: Contract
c2 = WithAmount 100 (One EUR)

-- "Ich zahle 100€ jetzt."
c3 :: Contract
c3 = WithChangedDirection c2

-- "Ich bekomme 100€ jetzt."
-- c4 = WithDirection Long c2

-- "Ich bekomme 100€ jetzt."
c5 :: Contract
c5 = WithChangedDirection c3

-- "Ich bekomme Weihnachten 100€."
zcb1 :: Contract
zcb1 = WithDate xmas (WithAmount 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency = WithDate date (WithAmount amount (One currency))

data Payment = MkPayment Date Direction Amount Currency
  deriving Show

-- operationelle Semantik
-- Zahlungen bis zu einem bestimmten Zeitpunkt + Residualvertrag
meaning :: Contract -> Date -> ([Payment], Contract)


c5 = WithAmount 100 (Two (One EUR) ()