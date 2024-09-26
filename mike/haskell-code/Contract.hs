module Contract where
{-
- einfaches Beispiel
  Zero-Bond / Zero-Coupon Bond
  "Ich bekomme am 24.12.2024 100€."
- aufspalten in "atomare Bestandteile" / "Ideen"
  - Datum
  - Betrag
  - Währung
- Kombinatoren formulieren
- wiederholen

"Am 24.12.2024:
- Ich bekomme 100€.
- Ich bezahle 100CHF."
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

data Direction = Long | Short
  deriving Show

data Contract =
    -- Ich bekomme einen Euro. Jetzt.
    One Currency
  | Amount Amount Contract
  | At Date Contract
  | Reverse Contract
  | Mix Contract Contract
  deriving Show

c1 :: Contract
-- "Ich bekomme jetzt einen Euro."
c1 = One EUR

-- Ich bekomme jetzt 100€.
c100 = Amount 100 (One EUR)

zcb1 :: Contract
zcb1 = At (Date "2024-12-24") (Amount 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    At date (Amount amount (One currency))

zcb1' :: Contract
zcb1' = zeroCouponBond (Date "2024-12-24") 100 EUR

-- "Ich bezahle Weihnachten 100CHF."
c2 :: Contract
c2 = Reverse (zeroCouponBond (Date "2024-12-24") 100 CHF)

swap1 :: Contract
swap1 = Mix zcb1 c2

fxSwap :: Date -> Amount -> Currency -> Amount -> Currency -> Contract
fxSwap date amount1 currency1 amount2 currency2 =
--    Mix (zeroCouponBond date amount1 currency1)
--        (Reverse (zeroCouponBond date amount2 currency2))
  At date (Mix (Amount amount1 (One currency1))
               (Reverse (Amount amount2 (One currency2))))