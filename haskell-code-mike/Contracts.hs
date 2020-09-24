module Contracts where

{-
Zero-Coupon Bond / Zero-Bond:

Ich bekomme am 31.12.2020 100€.
Ich bekomme am 31.12.2021 200GBP.

3 Ideen:

- später
- Währung
- Betrag

-}

data Date = Date String
  deriving (Show, Eq, Ord)

data Currency = EUR | GBP
  deriving (Show, Eq)

type Amount = Double

{-
data Contract =
    ZCB Date Amount Currency
  | Future -- ...
-}

data Contract =
    One Currency -- "Ich bekomme ein Euro jetzt."
  | Multiple Amount Contract
  | Later Date Contract
  | Give Contract
  | Combine Contract Contract -- Alle Rechte und Pflichten werden kombiniert
  | Zero
  deriving Show

-- Combine c Zero != c
-- aber: die "Semantik" ist die gleiche

zcb1 = later (Date "2020-12-31") (multiple 100 (one EUR))
zcb2 = later (Date "2021-12-31") (multiple 200 (one GBP))

one :: Currency -> Contract
one = One

-- smart constructor
multiple :: Amount -> Contract -> Contract
multiple amount Zero = Zero
multiple amount contract = Multiple amount contract

later :: Date -> Contract -> Contract
later = Later

-- Zero-Coupon-Bond konstruieren
zcb :: Date -> Amount -> Currency -> Contract
zcb date amount currency = later date (multiple amount (one currency))

-- Semantik: eindeutige Definition. was etwas bedeutet
-- denotational semantics: Syntax wird übersetzt in ein mathematisches Objekt
-- => "denotational design"

-- operationelle Semantik
-- "Syntax wird transformiert"

data Payment = Payment Date Amount Currency
  deriving (Show, Eq)

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (Payment date amount currency) =
    Payment date (factor * amount) currency

step :: Contract -> Date -> ([Payment], Contract)
step Zero now = ([], Zero)
step (One currency) now = 
    ([Payment now 1.0 currency], Zero)
step (Multiple amount contract) now =
    let (payments, residual) = step contract now
    in (fmap (scalePayment amount) payments, multiple amount residual)
step c@(Later date contract) now = 
    if now >= date
    then step contract now
    else ([], c)
step (Give contract) now =
    let (payments, residual) = step contract now
    in (fmap (scalePayment (-1)) payments, Give residual)
step (Combine contract1 contract2) now = 
    let (payments1, residual1) = step contract1 now
        (payments2, residual2) = step contract2 now
    in (payments1 ++ payments2, Combine residual1 residual2)
