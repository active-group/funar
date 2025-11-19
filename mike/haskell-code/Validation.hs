module Validation where

-- https://docs.hibernate.org/validator/9.1/reference/en-US/html_single/#_validating_constraints

data Car = MkCar { licensePlate :: LicensePlate, 
                   seatCount :: SeatCount }
 deriving Show

data LicensePlate = MkLicensePlate String
  deriving Show

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
