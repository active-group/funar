module Contract where

{-
Einfaches Beispiel:
"Ich bekomme am 24.12.2021 100EUR."
Zero-Coupon Bond

Beispiel zerlegen in "atomare Bestandteile"
- Währung: Ich bekomme 1 EUR jetzt.
- Menge/Vielfaches: Ich bekomme 100 EUR jetzt.
- Später: Ich bekomme 100 EUR später.

Suche nach Selbstreferenzen

Currency Swap:
Ich bekomme am 24.12.2021 100EUR - UND -
ich bezahle am 24.12.2021 100GBP.
-}

type Amount = Double 

data Currency = EUR | USD | GBP | YEN
  deriving Show

data Date = Date String 
  deriving (Show, Eq, Ord)

{-
data Contract =
    ZeroCouponBond Date Amount Currency

zcb1 = ZeroCouponBond (Date "2021-12-24") 100 EUR
-}

data Direction = Long | Short 
  deriving Show

data Contract =
    Zero
  | One Currency
  | Multiple Amount Contract
  | Later Date Contract
  | Reverse Contract
  | And Contract Contract
  deriving Show


instance Semigroup Contract where
    (<>) = And

instance Monoid Contract where
    mempty = Zero

zcb1 = Later (Date "2021-12-24") (Multiple 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    Later date (Multiple amount (One currency))

currencySwap :: Date -> Amount -> Currency -> Amount -> Currency -> Contract
currencySwap date receiveAmount receiveCurrency giveAmount giveCurrency =
    And (zeroCouponBond date receiveAmount receiveCurrency)
        (Reverse (zeroCouponBond date giveAmount giveCurrency))

zcb1' = zeroCouponBond (Date "2021-12-24") 100 EUR

-- Semantik

data Payment = Payment Direction Date Amount Currency
  deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor (Payment direction date amount currency) =
    Payment direction date (factor * amount) currency

reversePayment :: Payment -> Payment
reversePayment (Payment Long date amount currency) = Payment Short date amount currency
reversePayment (Payment Short date amount currency) = Payment Long date amount currency

contractPayments :: Contract -> Date -> ([Payment], Contract)
contractPayments Zero now = ([], Zero) 
contractPayments (One currency) now = ([Payment Long now 1 currency], Zero) 
contractPayments (Multiple amount contract) now =
    let (payments, residualContract) = contractPayments contract now
    in (map (scalePayment amount) payments, Multiple amount residualContract)
contractPayments (Reverse contract) now = 
    let (payments, residualContract) = contractPayments contract now
    in (map reversePayment payments, Reverse residualContract)
contractPayments (Later date contract) now = undefined 
contractPayments (And contract1 contract2) now = 
    let (payments1, residualContract1) = contractPayments contract1 now
        (payments2, residualContract2) = contractPayments contract2 now
    in (payments1 ++ payments2, And residualContract1 residualContract2)


c2 = And (zeroCouponBond (Date "2021-12-24") 100 EUR)
         (zeroCouponBond (Date "2022-12-24") 100 EUR)