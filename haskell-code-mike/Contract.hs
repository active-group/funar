module Contract where

{-
Wie modellieren?

- Einfaches Beispiel?

(Banker: 1. Versuch Future)

Zero-Coupon Bond / Zero-Bond:

D11: Receive 100GBP on 29 Jan 2001

Immer implizit 2 Vertragspartner.

- Beispiel in kleinere Bausteine / "Ideen" zerlegen

3 Ideen:
- "später"
- Betrag / Vielfaches
- Währung

- Suche den Kombinator! (... die Selbst-Referenz!)
  ... "die closure of operations"

- Weitere Beispiele ... wiederholen

Am 31.12.2021:
- Ich bekomme 100CHF UND
- ich zahle 100EUR.
("FX Swap")

Fehlt: zahlen statt bekommen

- Suche speziell einen binären Kombinator!
  c :: d -> d -> d
  (manchmal gibt's mehrere!)

  ... und damit eine Halbgruppe! (Assoziativgesetz!)

  Two a (Two b c) =~= Two (Two a b) c

   ... und dann auch noch nach einem neutralen Element!
   => Monoid
   
-}

type Amount = Double

data Currency = EUR | GBP | CHF
  deriving Show

data Date = Date String -- ISO-Notation
  deriving (Ord, Eq, Show)

{-
data Contract =
    ZeroCouponBond Amount Currency Date
  | Future
  | Annapurna
  | Himalaya
  | K2

{- 
Probleme:
- Muß immer erweitert werden
- Jeder neue Vertrag muß neu interpretiert werden
=> aufwendig & fehleranfällig
-}

-}

data Contract =
    Zero -- neutrales Element bzgl. Two
  | One Currency -- "Bekomme jetzt 1 EUR"
  | Multiple Amount Contract -- Currency
  | Later Date Contract
  -- dreht Zahlungsrichtungen um
  -- vertauscht Rechte und Pflichten
  | Pay Contract
  | Two Contract Contract
  deriving Show

zcb1 :: Contract
zcb1 = Later (Date "2001-01-29") (Multiple 100 (One GBP))

one currency = One currency

-- smart constructor
two :: Contract -> Contract -> Contract
two Zero contract2 = contract2
two contract1 Zero = contract1
two contract1 contract2 = Two contract1 contract2

zcb amount currency date =
    Later date (Multiple amount (One currency))

-- Am 31.12.2021 zahle ich 100EUR
c1 = Later (Date "2021-12-31") (Pay (Multiple 100 (One EUR)))

c2 = Pay (Pay (One EUR))
-- =~= One EUR
-- Pay (Pay c) =~= c
-- (nicht das gleiche Datenobjekt, bedeutet aber das gleiche)

-- Bekomme jetzt 100EUR
-- Multiple 100 (One EUR) 

fxSwap date amount1 currency1 amount2 currency2 =
   Two (zcb date amount1 currency1)
       (Pay (zcb date amount2 currency2))

-- Bedeutung / Semantik
-- denotationelle Semantik: Domänenobjekt |-> mathematisches Objekt
-- operationelle Semantik:  beschreibt das Verhalten

data Direction = Long | Short
  deriving Show

data Payment = Payment Direction Amount Currency Date  
  deriving Show

scalePayment :: Amount -> (Payment -> Payment)
scalePayment factor (Payment direction amount currency date) =
    Payment direction (factor * amount) currency date

r :: Direction -> Direction
r Short = Long
r Long = Short

reversePayment (Payment direction amount currency date) =
    let r Short = Long
        r Long = Short
    in Payment (r direction) amount currency date

-- es kommt raus: Zahlungen bis zu dem Datum, Residualvertrag

-- Semantik: Zwei Verträge c1, c2 sind gleich(bedeutend), wenn für jedes
-- mögliche Datum contractPayments c1 date == contractPayments c2 date
-- (modulo Listenreihenfolge)

contractPayments :: Contract -> Date -> ([Payment], Contract)
contractPayments Zero now = ([], Zero)
contractPayments (One currency) now = ([Payment Long 1 currency now], Zero)
contractPayments (Multiple amount contract) now =
    let (payments, residualContract) = contractPayments contract now
    in (map (scalePayment amount) payments, 
        Multiple amount residualContract)
contractPayments (Pay contract) now =
    let (payments, residualContract) = contractPayments contract now
    in (map reversePayment payments, Pay residualContract)
contractPayments c@(Later date contract) now =
    if date <= now
    then contractPayments contract now
    else ([], c)
contractPayments (Two Zero contract) now =
    contractPayments contract now
contractPayments (Two contract Zero) now =
    contractPayments contract now
contractPayments (Two contract1 contract2) now =
    let (payments1, residualContract1) = contractPayments contract1 now
        (payments2, residualContract2) = contractPayments contract2 now
    in (payments1 ++ payments2, two residualContract1 residualContract2)