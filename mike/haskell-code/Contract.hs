module Contract where

{-
Identifizieren, worum es geht - besondere "Dinge".

1. einfaches Beispiel
   Zero-Coupon Bond / Zero-Bond
   "Ich bekomme Weihnachten 100€."

2. Beispiel in "atomare" Bestandteile zerlegen
   - "später"
   - "Vielfaches"
   - "Währung"

   Einfachster Vertrag, in dem nur die Währung eine Rolle spielt.
   "Ich bekomme jetzt 1€."

   + "Vielfaches"
   "Ich bekomme jetzt 100€."

3. Nächstes Beispiel, wh.
   Currency-Swap
   Weihnachten bekomme ich 100€ UND zahle 90GBP.

fehlt: "und", "rein und raus"
-}

data Date = MkDate String -- YYYY-MM-DD
 deriving (Show, Eq, Ord)

xmas :: Date
xmas = MkDate "2025-12-24"
easter :: Date
easter = MkDate "2025-04-20"

type Amount = Double

data Currency = EUR | GBP | YEN | USD
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Call
  | Put
  | Future

zcb1 :: Contract
zcb1 = ZeroCouponBond xmas 100 EUR
-}

data Direction = Long | Short
  deriving Show

data Contract =
    Zero
  | One Currency
  | Multiple Amount Contract -- Selbstbezug ==> Kombinator
  | Later Date Contract
--  | Flow Direction Contract
  | Shorten Contract
  | And Contract Contract
  deriving Show

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 :: Contract
c2 = Multiple 100 (One EUR)

-- "Ich bekomme 5000€ jetzt."
c3 :: Contract
c3 = Multiple 50 c2

-- "Ich zahle 5000€ jetzt."
c4 :: Contract
c4 = Shorten c3

{-
-- "Ich bekomme 5000€ jetzt"
c5 :: Contract
c5 = Flow Short c4

-- "Ich bekomme 5000€ jetzt."
c6 :: Contract
c6 = Flow Long c3
-}

zcb1 :: Contract
zcb1 = Later xmas (Multiple 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
  Later date (Multiple amount (One currency))

zcb1' :: Contract
zcb1' = zeroCouponBond xmas 100 EUR

fxSwap1 :: Contract
fxSwap1 = Later xmas (And (Multiple 100 (One EUR))
                          (Shorten (Multiple 90 (One EUR))))

fxSwap1' = And (zeroCouponBond xmas 100 EUR)
               (Shorten (zeroCouponBond xmas 90 GBP))