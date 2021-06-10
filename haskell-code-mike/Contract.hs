module Contract where

{-
1. einfaches Beispiel

Zero-Coupon Bond / Zero-Bond:

Receive 100GBP on Jan 29 2001

2. möglichst direkter Datentyp

3. Datentyp in kleine Teile/Ideen zerhacken
- Betrag
- Währung
- Später

4. im Datentyp umsetzen

5. Selbstbezug suchen

6. Lücken stopfen
-}

type Amount = Double 

data Currency = EUR | GBP
  deriving Show

data Date = Date String
  deriving (Eq, Ord, Show)

{-
data Contract =
    ZeroCouponBond Amount Currency Date

zcb1 = ZeroCouponBond 100 GBP (Date "2001-01-29")

-}
data Contract =
    One Currency -- "ein Euro JETZT"
  | Multiple Amount Contract
  | Later Date Contract
  | Add Contract Contract -- Halbgruppe
  | Empty
  -- Möglichkeit: Or als Dual zu Add
  | Give Contract -- Vertrag umdrehen
  deriving Show

zcb1 = Later (Date "2001-01-29") (Multiple 100 (One GBP))

zeroCouponBond :: Amount -> Currency -> Date -> Contract
zeroCouponBond amount currency date =
    Later date (Multiple amount (One currency))

zcb1' = zeroCouponBond 100 GBP (Date "2001-01-29")
zcb2 = Give (zeroCouponBond 105 GBP (Date "2002-02-01"))

c1 = Add zcb1 zcb2

-- Bedeutung / Semantik

-- Welche Zahlungen in welcher Währung wann?

data Payment = Payment Amount Currency Date
  deriving Show

meaning :: Contract -> Date -> ([Payment], Contract)
 