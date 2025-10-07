module Contract where

{- vorweg: Vertrag zwischen zwei Parteien -}

{-
Vorgehensweise:

- einfaches Beispiel abfragen
  "Ich bekomme am 24.12.2025 100€."
  zero-coupon bond / Zero-Bond

- Beispiel zerlegen in "atomare Bestandteile" / "Ideen"
  geht häufig entlang der Attribute
  - Währung
  - Betrag
  - Datum

  Einfachster Vertrag nur mit Währung:
  "Ich bekomme 1€ jetzt."
  ... jetzt auch mit Betrag:
  "Ich bekomme 50€ jetzt."

- Nächstes Beispiel:
  Currency Swap / FxSwap
  "Ich bekomme am 24.12.2025 100€ und zahle 80GBP."
-}

data Currency = EUR | USD | GBP | YEN
  deriving Show

type Amount = Double

data Date = MkDate String
  deriving (Show, Eq, Ord) -- sorgt dafür, daß mit <= etc. vergleichbar

xmas2025 = MkDate "2025-12-24"
today = MkDate "2025-10-07"

-- >>> xmas2025 > today
-- True

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Call
  | Put
  | Future
  deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond xmas2025 100 EUR
-}

data Direction = Long | Short
  deriving Show

data Contract =
    Zero
  | One Currency
  | Scale Amount Contract
  | Later Date Contract -- "closure of operations"
--  | Direction Direction Contract
  | Reverse Contract
  | Combine Contract Contract
  deriving Show

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR


-- "Ich bekomme 50€ jetzt."
c2 = Scale 50 (One EUR)

-- "Ich bekomme 2500€ jetzt."
c3 = Scale 50 (Scale 50 (One EUR))

-- "Ich bekomme 50€ am 24.12.2025."
c4 = Later xmas2025 c2

zcb1 = Later xmas2025 (Scale 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Scale amount (One currency))

zcb1' :: Contract
zcb1' = zeroCouponBond xmas2025 100 EUR

-- "Ich zahle 1€ jetzt"
c5 :: Contract
c5 = Reverse (One EUR)

-- "Ich bekomme 1€ jetzt."
-- c6 = Direction Long (One EUR)

-- "Ich bekomme 1€ jetzt."
c7 = Reverse (Reverse (One EUR))
-- "Ich zahle 1€ jetzt."
-- c8 = Direction Long  (Direction Short (One EUR))

-- "Ich bekomme am 24.12.2025 100€ und zahle 80GBP."
c8 = Later xmas2025 (Combine (Scale 100 (One EUR))
                             (Reverse (Scale 80 (One GBP))))

c8' = Combine (zeroCouponBond xmas2025 100 EUR)
              (Reverse (zeroCouponBond xmas2025 80 GBP))

fxSwap :: Date -> Amount -> Currency -> Amount -> Currency -> Contract
fxSwap date myAmount myCurrency yourAmount yourCurrency =
    Later date (Combine (Scale myAmount (One myCurrency))
                        (Reverse (Scale yourAmount (One yourCurrency))))


data Payment =
    MkPayment Date Direction Amount Currency
    deriving Show

c9 = Scale 100 (Combine (One EUR) (Later xmas2025 (One EUR)))

-- Semantik
-- Zahlungen bis zu Datum ("today")
-- + "Residualvertrag"
meaning :: Contract -> Date -> ([Payment], Contract)