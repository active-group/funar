module Contract where

import Prelude hiding (and)

-- Annahme: Vertrag zwischen zwei impliziten Vertragspartnern
-- Einer bin ich.
-- Zero Coupon Bond
-- Am 24.12.2019 bekomme ich 100 EUR
-- Idee: Vertrag repräsentieren durch ein Objekt,
-- das dessen Struktur repräsentiert

-- 1. Versuch:
-- data Contract = Zcb Date Amount Currency | Future ... | Forward | ...

data Currency = EUR | GBP

-- Besser: Beispiele in kleinste Bestandteile zerlegen
type Amount = Double

type Date = String

data Contract =
    Zero -- neutrales Element bzgl. And
  | One Currency -- "Ich bekomme einen EURO jetzt"
  | Multiple Amount Contract -- nicht: Currency
  | Later Date Contract
  | And Contract Contract
  | Give Contract

-- Am Ende brauchen:

zcb :: Date -> Amount -> Currency -> Contract
zcb date amount currency =
    Later date (Multiple amount (One currency))

and = And

-- "smart constructor"
give Zero = Zero
give contract' = Give contract'

currencySwap :: Date -> Amount -> Currency -> Amount -> Currency -> Contract
currencySwap date amount1 currency1 amount2 currency2 =
    (zcb date amount1 currency1) `and`
     (give (zcb date amount2 currency2))

    -- Later date (Multiple amount1 (One currency1))
    -- Later date (Multiple amount2 (One currency2))

-- Halbgruppe?
-- (c1 `and` c2) `and` c3 = c1 `and` (c2 `and` c3)
-- gleich, aber "gleichbedeutend"

data Direction = Long | Short

invert Long = Short
invert Short = Long

data Payout = Payout Date Direction Amount Currency

invertPayout (Payout date direction amount currency) =
    Payout date (invert direction) amount currency

scalePayout factor (Payout date direction amount currency) =
    Payout date direction (factor * amount) currency

-- Ein Schritt des Lebenszyklus: liefert u.a. "residualen Vertrag"
step :: Contract -> Date -> ([Payout], Contract)
step Zero date = undefined
step (One currency) date =
    ([Payout date Long 1 currency], Zero)
step (Multiple factor contract') date =
    let (payouts, contractAfter) = step contract' date
    in (map (scalePayout factor) payouts, Multiple factor contractAfter)
step contract@(Later date' contract') date =
    if date' <= date -- Datum erreicht
    then step contract' date
    else ([], contract)
step (And contract1 contract2) date =
    let (payouts1, contractAfter1) = step contract1 date
        (payouts2, contractAfter2) = step contract2 date
    in (payouts1 ++ payouts2, And contractAfter1 contractAfter2)
step (Give contract') date =
    let (payouts, contractAfter) = step contract' date
    in (map invertPayout payouts, give contractAfter)