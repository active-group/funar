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
  | Zero
  | Amount Amount Contract
  | At Date Contract
  | Reverse Contract
  | Mix Contract Contract
  deriving Show

-- "smart constructor"
mix :: Contract -> Contract -> Contract
mix Zero c = c
mix c Zero = c
mix c1 c2 = Mix c1 c2

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


data Payment = MkPayment Direction Date Amount Currency
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
  MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment Long date amount currency) = MkPayment Short date amount currency
invertPayment (MkPayment Short date amount currency) = MkPayment Long date amount currency

instance Semigroup Contract where
    (<>) = mix

instance Monoid Contract where
    mempty = Zero

-- "alle Zahlungen bis heute" + Residualvertrag
semantics :: Contract -> Date -> ([Payment], Contract)
semantics (One currency) now = ([MkPayment Long now 1 currency], mempty)
semantics (Amount amount contract) now =
  let (payments, residualContract) = semantics contract now
   in (map (scalePayment amount) payments, Amount amount residualContract)
semantics c@(At date contract) now =
  if now >= date
    then semantics contract now
    else ([], c)
semantics (Reverse contract) now =
  let (payments, residualContract) = semantics contract now
   in (map invertPayment payments, Reverse residualContract)
semantics (Mix contract1 contract2) now =
  let (payments1, residualContract1) = semantics contract1 now
      (payments2, residualContract2) = semantics contract2 now
   in (payments1 <> payments2, residualContract1 <> residualContract2)
semantics Zero now = (mempty, mempty)

-- Monoiden-Homomorphismus

-- >>> semantics c (Date "2024-09-26")
-- ([MkPayment Long (Date "2024-09-26") 100.0 EUR],Amount 100.0 (At (Date "2024-12-24") (One EUR)))

c :: Contract
c = Amount 100 (Mix (One EUR) (At (Date "2024-12-24") (One EUR)))
