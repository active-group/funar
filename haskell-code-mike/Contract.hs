module Contract where

-- Zero-Coupon Bond
-- "Ich bekomme Weihnachten 12 EUR"

-- 1. Idee: direkt modellieren
-- 2. Idee: in kleinste Teile zertrümmern
-- 3. Idee: verallgemeinern - durch Kombinatoren
-- 4. Idee: möglichst Typen recyclen

type Date = String
data Currency = EUR | GBP deriving Show

data Contract =
    -- Zcb Date Double Currency
    -- Future
    -- Option
     Zero 
   | One Currency -- 1 EUR oder 1 GBP JETZT
   | Multiple Double Contract  -- <- Kombinator, JETZT
   | Later Date Contract -- JETZT Vertrag, SPÄTER einen anderen Vertrag abzuschließen
   | Both Contract Contract
   | Give Contract -- "dreht Vorzeichen"
   -- weitere Ideen: Or, Cond
   deriving Show

zcb :: Date -> Double -> Currency -> Contract
zcb date amount currency = Later date (Multiple amount (One currency))


data Payment = Payment Date Double Currency

scalePayment :: Double -> Payment -> Payment
scalePayment factor (Payment date amount currency) = Payment date (factor * amount) currency

-- Zahlungen bis zu einem bestimmten Zeitpunkt
step :: Contract -> Date -> ([Payment], Contract)
step Zero date = ([], Zero)
step (One currency) date =
    ([Payment date 1.0 currency], Zero)
step (Multiple factor contract') date =
    let (payments', residualContract) = step contract' date
    in (fmap (scalePayment factor) payments', Multiple factor residualContract)
step contract@(Later date' contract') date =
    if date >= date'
    then step contract' date
    else ([], contract)
step (Both contract1 contract2) date =
    let (payments1, residualContract1) = step contract1 date
        (payments2, residualContract2) = step contract2 date
    in (payments1 ++ payments2, Both residualContract1 residualContract2)
step (Give contract') date =
    let (payments', residualContract) = step contract' date
    in (fmap (scalePayment (-1)) payments', Give residualContract)

