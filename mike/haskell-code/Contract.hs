module Contract where

{-
Identifizieren, worum es geht - besondere "Dinge".

1. einfaches Beispiel
   Zero-Coupon Bond / Zero-Bond
   "Ich bekomme Weihnachten 100€."
-}

data Date = MkDate String -- YYYY-MM-DD
 deriving (Show, Eq, Ord)

xmas = MkDate "2025-12-24"
