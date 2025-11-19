{-# LANGUAGE InstanceSigs #-}
module Validation where

-- https://docs.hibernate.org/validator/9.1/reference/en-US/html_single/#_validating_constraints

data Car = MkCar { licensePlate :: LicensePlate, 
                   seatCount :: SeatCount }
 deriving Show

data LicensePlate = MkLicensePlate String
  deriving Show

mkLicensePlate :: String -> Validated LicensePlate
mkLicensePlate s =
    if length s >= 3 && length s <= 14
    then Valid (MkLicensePlate s)
    else Invalid ["wrong length"]

mkSeatCount :: Integer -> Validated SeatCount
mkSeatCount n =
    if n >= 2
    then Valid (MkSeatCount n)
    else Invalid ["wrong #seats"]

data SeatCount = MkSeatCount Integer
  deriving Show

data Validated a =
    Valid a
  | Invalid [String]
  deriving Show

instance Functor Validated where
    fmap :: (a -> b) -> Validated a -> Validated b
    fmap f (Valid a) = undefined
    fmap f (Invalid errors) = undefined

instance Applicative Validated where
    pure :: a -> Validated a
    pure a = Valid a

    (<*>) :: Validated (a -> b) -> Validated a -> Validated b
    (<*>) (Invalid errors1) (Invalid errors2) =
        Invalid (errors1 ++ errors2)
    (<*>) (Valid f) (Invalid errors) = Invalid errors
    (<*>) (Invalid errors) (Valid a) = Invalid errors
    (<*>) (Valid f) (Valid a) = Valid (f a)
