module Contract where

{-
1. einfaches Beispiel
Zero-Bond / Zero Coupon Bond
"Ich bekomme am 24.12.2022 100€."

2. Beispiel zerlegen in "atomare Bestandteile" / "Ideen"
   (anfangen mit einzelnen Attributen)

- Währung ("Ich bekomme 1€ jetzt!")
- Betrag ("Ich bekomme jetzt 100€")
- Später

Currency swap:
   Am 24.12.2022:
   - ich bekomme 100€
   - ich bezahle 150GBP
-}

data Date = MkDate String deriving (Eq, Ord, Show)

type Amount = Float

data Currency = EUR | GBP | USD | YEN
  deriving Show
{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Future Date -- ...
  | Put
  | Call
    deriving Show

zcb1 :: Contract
zcb1 = ZeroCouponBond (MkDate "2022-12-24") 100 EUR
-}

data Contract =
    One Currency
  | Many Amount Contract  -- <<< Selbstbezug
  | Later Date Contract
  | Give Contract
  | And Contract Contract -- Halbgruppe
  | Zero -- neutrale Element
  deriving Show

c1 = One EUR -- "Ich bekomme 1€ jetzt."
c2 = Many 100 (One EUR) -- "Ich bekomme 100€ jetzt."

c3 = Many 10 (Many 20 (One EUR)) -- 200€

zcb1 = Later (MkDate "2022-12-24") (Many 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Many amount (One currency))

zcb1' = zeroCouponBond (MkDate "2022-12-24") 100 EUR

zcb2 = Give (zeroCouponBond (MkDate "2022-12-24") 150 GBP)

currencySwap date amount1 currency1 amount2 currency2 =
    And (zeroCouponBond date amount1 currency1)
        (Give (zeroCouponBond date amount2 currency2))

-- im Paper:
-- scale :: Obs Double -> Contract -> Contract

-- haben Syntax
-- brauchen Semantik