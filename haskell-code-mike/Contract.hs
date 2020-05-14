module Contract where

-- Zero-Coupon Bond
-- "Ich bekomme Weihnachten 12 EUR"

-- 1. Idee: direkt modellieren
-- 2. Idee: in kleinste Teile zertrümmern
-- 3. Idee: verallgemeinern - durch Kombinatoren

type Date = String
data Currency = EUR | GBP deriving Show

data Contract =
    -- Zcb Date Double Currency
    -- Future
    -- Option
     One Currency -- 1 EUR oder 1 GBP JETZT
   | Multiple Double Contract  -- <- Kombinator, JETZT
   | Later Date Contract -- JETZT Vertrag, SPÄTER einen anderen Vertrag abzuschließen
   deriving Show

zcb :: Date -> Double -> Currency -> Contract
zcb date amount currency = Later date (Multiple amount (One currency))