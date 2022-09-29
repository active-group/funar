module Contract where

{-

1. einfaches Beispiel

Zero-Bond / Zero-Coupon Bond
"Ich bekomme am 24.12.2022 100€."

2. in "atomare Bestandteile" / Ideen zerlegen
- Währung ("Ich bekomme 1€ jetzt.")
- Vielfaches
- Später

3. Wiederholen

"Currency Swap": Weihnachten: Ich bekomme 100€ und ich zahle 100 Pfund.
-}

data Date = Date String deriving (Eq, Ord, Show)

christmas = Date "2022-12-24"

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Future
  | Call 
  | Put
    deriving Show

-- "Ich bekomme am 24.12.2022 100€"
zcb1 = ZeroCouponBond christmas 100 EUR

-}

data Direction = Long | Short
  deriving Show

data Contract =
    -- "Ich bekomme 1€ jetzt."
    One Currency
    -- "Ich bekomme 100€ jetzt."
  | Multiple Amount Contract
  | Later Date Contract
  -- "Ich zahle ..."
--  | WithDirection Direction Contract
  -- 
  | Reverse Contract
  | And Contract Contract
  | Zero
  deriving Show

instance Semigroup Contract where
    (<>) = And

-- "Ich bekomme 100€ jetzt."
c1 = Multiple 100 (One EUR)
zcb1 = Later christmas (Multiple 100 (One EUR))

-- "Ich bezahle Weihnachten 100€."
c2 = Reverse zcb1

zeroCouponBond date amount currency =
    Later date (Multiple amount (One currency))
zcb1' = zeroCouponBond christmas 100 EUR

currencySwap date amount1 currency1 amount2 currency2 =
    And (zeroCouponBond date amount1 currency1)
        (Reverse (zeroCouponBond date amount2 currency2))

-- schön wäre noch: Semantik

data Payment = MkPayment Direction Date Amount Currency
  deriving Show

semantics :: Contract -> Date -> ([Payment], Contract)
-- let (payments, residualContract) = semantics contract date
-- in ...
semantics Zero now =
  ([], Zero)
semantics (One currency) now =
  ([MkPayment Long now 1 currency], Zero)
semantics (Multiple amount contract) now =
  let (payments, contract') = semantics contract now
   in (map (multiplyPayment amount) payments, Multiple amount contract')
semantics (Reverse contract) now =
  let (payments, contract') = semantics contract now
   in (map invertPayment payments, Reverse contract')
semantics (And contract1 contract2) now =
  let (payments1, contract1') = semantics contract1 now
      (payments2, contract2') = semantics contract2 now
   in (payments1 ++ payments2, Contract.and contract1' contract2')
semantics c@(Later date contract) now =
  if now >= date
    then -- Weihnachten
      semantics contract now
    else -- noch nicht Weihnachten
      ([], c)

multiplyPayment :: Amount -> Payment -> Payment
multiplyPayment factor (MkPayment dir date amount currency) =
  MkPayment dir date (factor * amount) currency

invertPayment :: Payment -> Payment
invertPayment (MkPayment Long date amount currency) =
  MkPayment Short date amount currency
invertPayment (MkPayment Short date amount currency) =
  MkPayment Long date amount currency

-- >>> semantics c4 (Date "2022-10-01")
-- ([MkPayment Long (Date "2022-10-01") 1.0 EUR],Later (Date "2022-12-24") (Multiple 1.0 (One EUR)))
c4 = And (One EUR) (zeroCouponBond christmas 1 EUR)

-- smart constructor
and :: Contract -> Contract -> Contract
and Zero c = c 
and c Zero = c
and c1 c2 = And c1 c2
