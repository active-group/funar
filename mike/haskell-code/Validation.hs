{-# LANGUAGE InstanceSigs #-}
module Validation where

-- "Make illegal states unrepresentable." -- Yaron Minsky

-- 1. möglichst viele Einschränkungen in den Typen festhalten

-- 2 bis 14 Buchstaben
data LicensePlate = MkLicensePlate String
  deriving Show

-- mindestens 2
data SeatCount = MkSeatCount Integer
  deriving Show

data Car = MkCar { licensePlate :: LicensePlate,
                   seatCount :: SeatCount }
    deriving Show

data Result a =
    Success a
  | Failure [String] -- Fehlermeldungen
  deriving Show

-- weil Typparameter
mapResult :: (a -> b) -> Result a -> Result b
mapResult f (Success a) = Success (f a)
mapResult f (Failure errors) = Failure errors

instance Functor Result where
    fmap :: (a -> b) -> Result a -> Result b
    fmap = mapResult

-- applikativer Funktor
-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

mkSeatCount :: Integer -> Result SeatCount
mkSeatCount n =
    if n >= 2
    then Success (MkSeatCount n)
    else Failure ["seat count too small"]

mkLicensePlate :: String -> Result LicensePlate
mkLicensePlate s =
    let l = length s
    in if l >= 2 && l <= 14
       then Success (MkLicensePlate s)
       else Failure ["invalid license-plate length"]

mkCar :: String -> Integer -> Result Car
mkCar s n =
    {-
    case mkLicensePlate s of
        Just licensePlate ->
            case mkSeatCount n of
                Just seatCount ->
                    Just (MkCar licensePlate seatCount)
                Nothing -> Nothing
        Nothing -> Nothing
-}
    case (mkLicensePlate s, mkSeatCount n) of
        (Success licensePlate, Success seatCount) ->
            Success (MkCar licensePlate seatCount)
        (Failure errors, Success _) -> Failure errors
        (Success _, Failure errors) -> Failure errors
        (Failure errors1, Failure errors2) -> Failure (errors1 ++ errors2)

-- Wunschliste:
-- - Fehlermeldungen
-- - n-stelliges fmap
