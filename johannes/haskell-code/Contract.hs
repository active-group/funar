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
    | MultiplyWith Amount Contract -- <- Selbstbezug
    | Delay Date Contract
    deriving Show

-- >>> :t One
-- One :: Currency -> Contract

-- >>> MultiplyWith 30 (One EUR)
-- MultiplyWith 30.0 (One EUR)

-- >>> Tomorrow (Tomorrow (One EUR))
-- Tomorrow (Tomorrow (One EUR))

-- es geht: - um mich
zcb1 :: Contract
zcb1 = Delay (MkDate "24.12.2023") (MultiplyWith 100 (One EUR))

-- Zero-coupon bond erstellen
zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount curr = Delay date (MultiplyWith amount (One curr))

-- currencySwap :: Contract
-- currencySwap = undefined