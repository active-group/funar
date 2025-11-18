module Contract where

{-
Identifizieren, worum es geht - besondere "Dinge".

1. einfaches Beispiel
   Zero-Coupon Bond / Zero-Bond
   "Ich bekomme Weihnachten 100€."
-}

data Date = MkDate String -- YYYY-MM-DD
 deriving (Show, Eq, Ord)

xmas :: Date
xmas = MkDate "2025-12-24"
easter :: Date
easter = MkDate "2025-04-20"
