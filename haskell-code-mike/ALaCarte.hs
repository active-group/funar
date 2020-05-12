{-# LANGUAGE TypeOperators #-}

module ALaCarte where

data Expr f = In (f (Expr f))

data Val e = Val Int
data Add e = Add e e

-- Produkt: zusammgesetzte Daten
-- kartesische Produkt - Tupel
-- X ist das Produkt von Int und String
-- Int x String, F# int * string
type X = (Int, String)

-- Coprodukt: gemischte Daten - |

data (f :+: g) e = Inl (f e) | Inr (g e)


