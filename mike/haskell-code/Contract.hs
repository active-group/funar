module Contract where

{-
Vertrag implizit zwischen Bank und Kund:in

1. einfaches Beispiel
Zero-Bond / zero-coupon bond
"Ich bekomme 100€ am 24.12.2024."

2. Beispiel in "atomare Bestandteile"/Ideen zerlegen
- Betrag
- Währung
- Später
... mit Selbstbezügen / Kombinatoren

3. wiederholen, mit größeren Beispiel
Currency Swap:
- Ich bekomme am 24.12.2024 100€ und
- ich bezahle am selben Tag 100USD.
-}

type Amount = Double

data Currency = EUR | GBP | YEN | USD
  deriving Show

data Date = MkDate String
  deriving (Eq, Ord, Show)

{-
-- blöd:
data Contract =
    ZeroCouponBond Amount Currency Date
  | Call 
  | Put 
  | Himalaya 
  | Butterblume

zcb1 = ZeroCouponBond 100 EUR (MkDate "2024-12-24")
-}

data Contract =
    -- Währung
    -- "Ich bekomme 1€ jetzt."
    One Currency
    -- Betrag
    -- "Ich bekomme 5€ jetzt."
  | Times Amount Contract -- Selbstbezug
    -- später
  | AtDueDate Date Contract
  | And Contract Contract
  | Negate Contract -- dreht alle Zahlungsrichtungen um
  deriving Show

-- Ich bekomme 1€ jetzt
c1 = One EUR

c5 = Times 5 (One EUR)

--- "Ich bekomme am 24.12.2024 100€."
zcb1 = AtDueDate (MkDate "2024-12-24") (Times 100 (One EUR))

-- "Ich bezahle am 24.12.2024 100€."
zcb1r = Pay zcb1


zcb1rr = Pay zcb1r

cx = Receive zcb1

c2 = AtDueDate (MkDate "2024-02-01") (AtDueDate (MkDate "2024-02-02") (Times 5 (One EUR)))

zeroCouponBond :: Amount -> Currency -> Date -> Contract
zeroCouponBond amount currency date = AtDueDate date (Times amount (One currency))

zcb1' = zeroCouponBond 100 EUR (MkDate "2024-12-24")
