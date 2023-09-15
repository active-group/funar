module Contract where
{-
- einfaches Beispiel
Zero-Bond  / Zero-Coupon Bond
"Ich bekomme am 24.12.2023 100€."

- Beispiel in "atomare Bestandteile" / "Ideen"

1. "Währung" - "Ich bekomme 1€ jetzt."
2. "Betrag" - "Ich bekomme 100€ jetzt."
3. "Später"

- Bausteine mit Selbstbezügen machen

- Wiederholen, mit weiteren Beispielen.

Currency Swap:
Am 24.12.2023:
sowohl "Ich bekomme 100€"
als auch "Ich zahle 100$".
-}

data Date = Date String
  deriving (Eq, Ord, Show)

type Amount = Double

data Currency =
    EUR | USD | GBP | YEN
    deriving Show
{-
data Contract =
    ZeroCouponBond Date Amount Currency
    deriving Show

-- Ich bekomme am 23.12.2023 100€.
zcb1 = ZeroCouponBond (Date "2023-12-23") 100 EUR
-}

data Direction = Long | Short deriving Show

data Contract =
    One Currency
  | WithAmount Amount Contract -- Selbstbezug
  | DueDate Date Contract
  | Invert Contract -- dreht alle Zahlungen um
  | Together Contract Contract
  | Zero
  deriving Show

-- im Paper: Obs Amount statt Amount

-- Together assoziativ? Ja!
-- Verträge bilden eine Halbgruppe! Und einen Monoid!

c1 = One EUR -- "Ich bekomme einen Euro jetzt."

c2 = WithAmount 100 (One EUR) -- "Ich bekomme 100€ jetzt."

zcb1 = DueDate (Date "2023-12-23") (WithAmount 100 (One EUR))

zeroCouponBond :: Date -> Amount -> Currency -> Contract
zeroCouponBond date amount currency =
    DueDate date (WithAmount amount (One currency))

zcb1' = zeroCouponBond (Date "2023-12-23") 100 EUR

-- Ich zahle Weihnachten $100
c3 = Invert (zeroCouponBond (Date "2023-12-23") 100 USD)

fxSwap :: Contract
fxSwap = Together zcb1 c3

-- Semantik

data Payment = MkPayment {
    paymentDate :: Date,
    paymentDirection :: Direction,
    paymentAmount :: Amount,
    paymentCurrency :: Currency
 }
 deriving Show

scalePayment :: Amount -> Payment -> Payment
scalePayment factor payment = payment { paymentAmount = paymentAmount payment * factor}

invertDirection :: Direction -> Direction
invertDirection Long = Short
invertDirection Short = Long

invertPayment :: Payment -> Payment
invertPayment payment =
    payment { paymentDirection = invertDirection (paymentDirection payment)}

f x =
    let y = x +1
        z = y * 2
        (a, b) = foo
    in x + y + z + a + b

foo = (23, 42)

-- Welche Zahlungen bis heute?
-- Und welcher Vertrag bleibt übrig?
semantics :: Contract -> Date -> ([Payment], Contract)
semantics (One currency) now = ([MkPayment now Long 1 currency], Zero)
semantics (WithAmount amount contract) now =
  let (payments, residualContract) = semantics contract now
   in (map (scalePayment amount) payments, WithAmount amount residualContract)
semantics c@(DueDate date contract) now =
  if now >= date
    then semantics contract now
    else ([], c)
semantics (Invert contract) now =
  let (payments, residualContract) = semantics contract now
   in (map invertPayment payments, Invert residualContract)
semantics (Together contract1 contract2) now =
  let (payments1, residualContract1) = semantics contract1 now
      (payments2, residualContract2) = semantics contract2 now
   in (payments1 ++ payments2, Together residualContract1 residualContract2)
semantics Zero now = ([], Zero)

-- smart constructor
together :: Contract -> Contract -> Contract
together Zero contract = contract
together contract Zero = contract
together contract1 contract2 = Together contract1 contract2

-- >>> semantics c5 (Date "2023-09-15")
c5 = WithAmount 100 (Together (One EUR) (DueDate (Date "2023-12-25") (One EUR)))