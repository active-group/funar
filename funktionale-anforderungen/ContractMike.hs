module Contract where

-- Kombinatoren
-- f :: Dingsda -> Dingsda -> Dingsda
-- Beispiel: beside, overlay

-- Gegenentwurf:
-- f :: Dingsda -> Dingsda -> Dungsda

-- "Verträge werden sofort fällig"
data Contract =
  -- Zcb Date Double Currency -- netter Versuch
    Zero
  | One Currency -- "ich bekomme 1 EUR"
  | Scale Double Contract
  | Later Date Contract
  | And Contract Contract
  | Give Contract

-- Zero-Coupon Bond
-- spätere Zahlung eines Betrags

data Currency = EUR | GBP

data Date = Date String | Never
  deriving (Eq, Ord)

never = Never

zcb :: Date -> Double -> Currency -> Contract
zcb date amount currency =
  Later date (Scale amount (One currency))

c1 = zcb (Date "2019-12-24") 100 GBP
c2 = zcb (Date "2020-12-24") 1000 EUR
c3 = c1 `andC` c2

-- “smart constructor"
andC :: Contract -> Contract -> Contract
andC Zero contract = contract
andC contract Zero = contract
andC contract1 contract2 = And contract1 contract2

--- Scale x Zero ="äquivalent"= Zero
-- Zwei Verträge sind dann äquivalent, wenn sie die gleichen Zahlungen veranlassen.
scale :: Double -> Contract -> Contract
scale amount Zero = Zero
scale amount contract = Scale amount contract

later :: Date -> Contract -> Contract
later date Zero = Zero
later date contract = Later date contract

give :: Contract -> Contract
give Zero = Zero
give contract = Give contract

type ResidualContract = Contract
type NextDate = Date

data Payment = Payment Date Double Currency

paymentScale :: Double -> Payment -> Payment
paymentScale factor (Payment date amount currency) =
  Payment date (amount * factor) currency

step :: Contract -> Date -> ([Payment], ResidualContract, NextDate)
step Zero date =
  ([], Zero, never)
step (One currency) date =
  ([Payment date 1 currency], Zero, never)
step (Scale amount' contract') date =
  let (payments, residual, next) = step contract' date
  in (map (paymentScale amount') payments,
      Scale amount' residual,
      next)
step contract@(Later date' contract') date =
  if date' <= date -- vorbei!
  then step contract' date
  else ([], contract, date')
step (And contract1 contract2) date =
  let (payments1, residual1, next1) = step contract1 date
      (payments2, residual2, next2) = step contract2 date
  in (payments1 ++ payments2, andC residual1 residual2, min next1 next2)
step (Give contract') date =
  let (payments, residual, next) = step contract' date
  in (map (paymentScale (-1)) payments,
      Give residual,
      next)

c4 = Scale 100 (One GBP `andC` (Later (Date "2019-12-24") (One EUR)))