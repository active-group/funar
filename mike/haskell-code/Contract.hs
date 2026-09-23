module Contract where

{-
- simple example
  Zero-Coupon Bond: "I receive 100€ on 2026-12-24."
- split example into "atomic parts"
  - currency: "I receive 1€ now."
  - amount: "I receive 100€ now."
  - later:
- put in self-references, make combinators
- repeat with more examples

Currency swap:
On Christmas, I receive 100€ and I pay $100.
-}

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

data Date = MkDate String
  deriving (Show, Eq, Ord)

{-
data Contract =
    ZeroCouponBond Amount Currency Date
    deriving Show
-}

data Contract =
    Zero
  | One Currency
  | Many Amount Contract -- Currency
  | Later Date Contract
  | Composite Contract Contract
  | Deposit Contract
  deriving Show

-- "I get 1€ now."
c1 :: Contract
c1 = One EUR

-- "I get 100€ now."
c2 :: Contract
c2 = Many 100 (One EUR)

xmas :: Date
xmas = MkDate "2026-12-24"

c3 :: Contract
c3 = Later xmas (Many 100 (One EUR))

zeroCouponBond date amount currency =
    Later date (Many amount (One currency))

zcb1 :: Contract
zcb1 = zeroCouponBond xmas 100 EUR

-- "I receive 2000€ now."
c4 = Many 100 (Many 20 (One EUR))

easter = MkDate "2027-03-31"

-- "I receive 20€ on easter."
c5 = Later xmas (Many 20 (Later easter (One EUR)))

-- "I receive 20€ on easter."
c6 = Later easter (Many 20 (Later xmas (One EUR)))

-- "I pay 1€."
c7 = Deposit (One EUR)

-- "I receive 1€."
c8 = Deposit (Deposit (One EUR))

c9 = Later xmas (Composite (Many 100 (One EUR))
                           (Deposit (Many 100 (One GBP))))

fxSwap date amount1 currency1 amount2 currency2 =
    Composite (zeroCouponBond date amount1 currency1)
              (Deposit (zeroCouponBond date amount2 currency2))