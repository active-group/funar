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

data Direction = Long | Short
  deriving Show

data Payment = MkPayment Direction Date Amount Currency
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
    MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment Long date amount currency) = MkPayment Short date amount currency
invertPayment (MkPayment Short date amount currency) = MkPayment Long date amount currency

-- Datum: "jetzt" bzw. "Zahlungen bis jetzt."
semantics :: Contract -> Date -> ([Payment], Contract) -- "Residualvertrag"
semantics (One currency) now = ([MkPayment Long now currency], Zero)
semantics (Many amount contract) now =
    let (payments, residualContract) = semantics contract now
    in (map (scalePayment amount) payments, Many amount residualContract)
semantics c@(Later date contract) now =
    if now >= date
    then semantics contract now
    else ([], c)
semantics (Give contract) now =
    let (payments, residualContract) = semantics contract now
    in (map invertPayment payments, Give residualContract)
semantics (And contract1 contract2) now =
    let (payments1, residualContract1) = semantics contract1 now
        (payments2, residualContract2) = semantics contract2 now
    in (payments1 ++ payments2, And contract1 contract2)
semantics Zero now = ([], Zero)