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