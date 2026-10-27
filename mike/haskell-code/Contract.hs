module Contract where

{-
- einfaches Beispiel
  Zero-Coupon-Bond / Zero-Bond
  "Ich bekomme am 24.12.2026 100€."

- Beispiel zerlegen in "atomare Bestandteile" / Bausteine / "Ideen"
  z.B. entlang der Attribute
  - Währung: "Ich bekomme 1€ jetzt."
  - Betrag
  - Später/Datum

- Currency-Swap:
  Weihnachten bekomme ich 100€ und zahle 100$.
-}

type Date = String  -- ISO-Format: YYYY-MM-DD

xmas :: Date
xmas = "2026-12-24"

isBefore :: Date -> Date -> Bool
isBefore s1 s2 = s1 <= s2

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Put
  | Call 
  | Future
  | Everest

zcb1 :: Contract
zcb1 = ZeroCouponBond xmas 100 EUR
-}

data Contract =
    Zero
  | One Currency
  | Value Amount Contract
  | Later Date Contract
  | Sell Contract
  | Combi Contract Contract
  deriving Show

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 :: Contract
c2 = Value 100 (One EUR)

-- "Ich bekomme Weihnachten 100€."
c3 :: Contract
c3 = Later xmas (Value 100 (One EUR))


-- "Ich bekomme Weihnachten 1€."
c4 :: Contract
c4 = Later xmas (One EUR)

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Value amount (One currency))

zcb1 :: Contract
zcb1 = zeroCouponBond xmas 100 EUR

-- "Ich zahle Weihnachten 100$"
c5 :: Contract
c5 = Sell (Later xmas (Value 100 (One USD)))

c5' :: Contract
c5' = Sell (zeroCouponBond xmas 100 USD)

fxSwap1 = Later xmas (Combi (Value 100 (One EUR))
                            (Sell (Value 100 (One USD))))