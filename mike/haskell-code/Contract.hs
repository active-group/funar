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

-- smart constructor
composite :: Contract -> Contract -> Contract
composite Zero c = c
composite c Zero = c
composite c1 c2 = Composite c1 c2

deposit :: Contract -> Contract
deposit Zero = Zero
deposit c = Deposit c

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

fxSwap :: Date -> Amount -> Currency -> Amount -> Currency -> Contract
fxSwap date amount1 currency1 amount2 currency2 =
    Composite (zeroCouponBond date amount1 currency1)
              (Deposit (zeroCouponBond date amount2 currency2))


data Direction = Incoming | Outgoing
  deriving Show

data Payment = MkPayment Date Direction Amount Currency
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (MkPayment direction date amount currency) =
  MkPayment direction date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment date Incoming amount currency) =
  MkPayment date Outgoing amount currency
invertPayment (MkPayment date Outgoing amount currency) =
  MkPayment date Incoming amount currency

instance Semigroup Contract where
    (<>) = composite

instance Monoid Contract where
    mempty = Zero
    
-- all payments until date (today)
-- returns payments, residual contract after the payments
semantics :: Contract -> Date -> ([Payment], Contract)
semantics Zero today = ([], Zero)
semantics (One currency) today = ([MkPayment today Incoming 1 currency], Zero)
semantics (Many amount contract) today =
  let (payments, residualContract) = semantics contract today
   in (map (scalePayment amount) payments, Many amount residualContract)
semantics (Later date contract) today =
  if today >= date
    then semantics contract today
    else ([], Later date contract)
semantics (Deposit contract) today =
  let (payments, residualContract) = semantics contract today
   in (map invertPayment payments, deposit residualContract)
semantics (Composite contract1 contract2) today =
  let (payments1, residualContract1) = semantics contract1 today
      (payments2, residualContract2) = semantics contract2 today
   in (payments1 ++ payments2, composite residualContract1 residualContract2)


-- >>> semantics c10 (MkDate "2026-09-23")
-- ([MkPayment (MkDate "2026-09-23") Incoming 100.0 EUR],Many 100.0 (Later (MkDate "2026-12-24") (One EUR)))
c10 = Many 100 (Composite (One EUR)
                          (Later xmas (One EUR)))

-- >>> semantics c11 (MkDate "2026-09-23")
-- ([MkPayment (MkDate "2026-09-23") Outgoing 100.0 EUR],Many 100.0 (Later (MkDate "2026-12-24") (One EUR)))
c11 =
  Many
    100
    ( Composite
        (Deposit (One EUR))
        (Later xmas (One EUR))
    )

