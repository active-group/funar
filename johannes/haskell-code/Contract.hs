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
    deriving Show

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

data Payment = MkPayment Direction Date Amount Currency

-- Datum: "Zahlungen bis jetzt"
semantics :: Contract -> Date -> ([Payment], Contract) -- Residualvertrag
semantics = undefined