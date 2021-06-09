module Cards where

-- Repräsentation (Typ) für Karte
-- kompletter Satz Karten
-- Funktion, die sagt, ob eine Karte höherwertig ist als eine andere
-- (geht nur für gleiche Farbe)

{-
data Maybe a = Nothing | Just a
-}

{-
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
-}