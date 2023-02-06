module Contract where

{-

1. einfaches Beispiel:

-- Zero-Coupon-Bond:

"Ich bekomme am 24.12.2023 100 Euro."

Ziel: Simplifizieren der Domäne

Bestandteile:
- Aktion
- Datum/Zeitpunkt
- Person
- Währung
- Menge

Currency swap:
  Am 24.12.2023:
    - Ich bekomme 100 Euro
    - Ich bezahle 150 GBP

-}

newtype Date = MkDate String
    deriving (Ord, Show, Eq)

data Currency = EUR | GBP | USD | YEN
    deriving (Eq, Show)

type Amount = Float

-- Ein Vertrag ist ...
-- 
-- data Contract
--     = ZeroCouponBond Date Amount Currency
--     | CurrencySwap Contract Contract

data Contract
    -- Ich bekomme 1 "Currency" _jetzt_.
    = One Currency
    | Multiply Amount Contract -- <- Selbstbezug
    | Delay Date Contract
    | Both Contract Contract
    | Negate Contract
    | Choice Contract Contract
    | Zero
    deriving Show

instance Semigroup Contract where
    (<>) = Both

instance Monoid Contract where
    mempty = Zero

-- >>> One EUR <> One GBP
-- Both (One EUR) (One GBP)

-- >>> :t One
-- One :: Currency -> Contract

-- >>> MultiplyWith 30 (One EUR)
-- MultiplyWith 30.0 (One EUR)

-- >>> Tomorrow (Tomorrow (One EUR))
-- Tomorrow (Tomorrow (One EUR))

-- es geht: - um mich
zcb1 :: Contract
zcb1 = Delay (MkDate "24.12.2023") (Multiply 100 (One EUR))

-- Zero-coupon bond erstellen
zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount curr = Delay date (Multiply amount (One curr))

currencySwap :: Date -> Amount -> Currency -> Amount -> Currency -> Contract
currencySwap date amount1 curr1 amount2 curr2 =
    Both (zeroCouponBond date amount1 curr1)
         (Negate (zeroCouponBond date amount2 curr2))



-- haben Syntax für Verträge
-- brauchen: Semantik

-- Long ist gut für mich!
data Direction = Long | Short
    deriving (Eq, Show)

-- Eine Zahlung besteht aus:
-- - Richtung
-- - Datum
-- - Menge
-- - Währung
data Payment = MkPayment
    { direction :: Direction
    , date :: Date
    , amount :: Amount
    , currency :: Currency
    }
    deriving (Eq, Show)

-- Richtung einer Zahlung umkehren
negatePayment :: Payment -> Payment
negatePayment payment = payment { direction = toggleDirection (direction payment) }
  where
    toggleDirection Long = Short
    toggleDirection Short = Long

-- Zahlung skalieren
scalePayment :: Amount -> Payment -> Payment
scalePayment factor payment =
  payment { amount = factor * (amount payment) }

-- Datum: "Zahlungen bis jetzt"
semantics :: Contract -> Date -> ([Payment], Contract) -- Residualvertrag
-- Damit man das Negate auch wieder los wird
semantics (Negate Zero) now = ([], Zero)
-- "Ausmultiplizieren bei Negate"
semantics (Negate contract) now =
    let (payments, restContract) = semantics contract now
     in (fmap negatePayment payments, Negate restContract)
semantics Zero _ = ([], Zero)
semantics (Multiply _ Zero) now = ([], Zero)
semantics (Multiply amount contract) now =
    let (payments, restContract) = semantics contract now
     --             v    nicht vergessen, dass der Restvertrag auch multipliziert werden muss (Eva: "Ausmultiplizieren")
     in (fmap (scalePayment amount) payments, Multiply amount restContract)
semantics contract@(Delay date innerContract) now =
    if now >= date
    then semantics innerContract now
    else ([], contract)
-- Damit man das Both auch wieder los wird
semantics (Both Zero Zero) = ([], Zero)
semantics (Both c1 c2) now =
  let (payments1, restContract1) = semantics c1 now
      (payments2, restContract2) = semantics c2 now
   in (payments1 <> payments2, Both restContract1 restContract2)
-- Können Choice hier nicht implementieren, da Rückfrage erforderlich ist
-- -> Siehe Paper
semantics _ _ = undefined
