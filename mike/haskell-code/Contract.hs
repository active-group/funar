module Contract where

{-
- einfaches Beispiel
  Zero-Coupon-Bond / Zero-Bond
  "Ich bekomme am 24.12.2026 100€."

- Beispiel zerlegen in "atomare Bestandteile" / Bausteine / "Ideen"
  z.B. entlang der Attribute
  - Währung: "Ich bekomme 1€ jetzt."
  - Betrag
  - Später/Datum

- Currency-Swap:
  Weihnachten bekomme ich 100€ und zahle 100$.
-}

type Date = String  -- ISO-Format: YYYY-MM-DD

xmas :: Date
xmas = "2026-12-24"

isBefore :: Date -> Date -> Bool
isBefore s1 s2 = s1 <= s2

type Amount = Double

data Currency = EUR | GBP | USD | YEN
  deriving Show

{-
data Contract =
    ZeroCouponBond Date Amount Currency
  | Put
  | Call 
  | Future
  | Everest

zcb1 :: Contract
zcb1 = ZeroCouponBond xmas 100 EUR
-}

data Contract =
    Zero
  | One Currency
  | Value Amount Contract
  | Later Date Contract
  | Sell Contract
  | Combi Contract Contract
  deriving Show

-- "Ich bekomme 1€ jetzt."
c1 :: Contract
c1 = One EUR

-- "Ich bekomme 100€ jetzt."
c2 :: Contract
c2 = Value 100 (One EUR)

-- "Ich bekomme Weihnachten 100€."
c3 :: Contract
c3 = Later xmas (Value 100 (One EUR))


-- "Ich bekomme Weihnachten 1€."
c4 :: Contract
c4 = Later xmas (One EUR)

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Value amount (One currency))

zcb1 :: Contract
zcb1 = zeroCouponBond xmas 100 EUR

-- "Ich zahle Weihnachten 100$"
c5 :: Contract
c5 = Sell (Later xmas (Value 100 (One USD)))

c5' :: Contract
c5' = Sell (zeroCouponBond xmas 100 USD)

fxSwap1 = Later xmas (Combi (Value 100 (One EUR))
                            (Sell (Value 100 (One USD))))

-- "Ich bekomme Weihnachten 100$."
c6 = Sell c5

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

-- Datum: "heute", Zahlungen bis heute
-- zurückgegeben wird der "Residualvertrag"
semantics :: Contract -> Date -> ([Payment], Contract)
semantics Zero today = mempty -- ([], Zero)
semantics (One currency) today = ([MkPayment today Incoming 1 currency], Zero)
semantics (Value amount contract) today =
  let (payments, residualContract) = semantics contract today
   in (map (scalePayment amount) payments, residualContract)
semantics (Later date contract) today =
  if today >= date
    then semantics contract today
    else ([], Later date contract)
semantics (Sell contract) today =
  let (payments, residualContract) = semantics contract today
   in (map invertPayment payments, residualContract)
semantics (Combi contract1 contract2) today =
  let (payments1, residualContract1) = semantics contract1 today
      (payments2, residualContract2) = semantics contract2 today
  in (payments1 ++ payments2, Combi residualContract1 residualContract2)
