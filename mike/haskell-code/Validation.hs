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
                   seatCount :: Integer }
    deriving Show

