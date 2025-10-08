module Validation where

-- Yaron Minsky:
-- "Make illegal states unrepresentable."

data Car = MkCar { carManufacturer :: Manufacturer,
                   carLicensePlate :: Licenseplate, -- 2-14 Buchstaben
                   carSeats :: SeatCount } -- mind. 2
    deriving Show

data Manufacturer = MkManufacturer String
  deriving Show

data Licenseplate = MkLicenseplate String
  deriving Show

data SeatCount = MkSeatCount Integer
  deriving Show

makeLicenseplate :: String -> Validated Licenseplate
makeLicenseplate text =
    let l = length text
    in if 2 <= l && l <= 14
        then Valid (MkLicenseplate text)
        else Invalid ["wrong length for license plate"]

makeSeatCount n =
    if n >= 2
    then Valid (MkSeatCount n)
    else Invalid ["not enough seats"]

-- if-Kaskade
-- keine Fehlermeldung

data Validated a =
    Valid a
  | Invalid [String] -- Fehlermeldung

{-
makeCar :: String -> String -> Integer -> Maybe Car
makeCar manufacturer licensePlate seats =
    let l = length licensePlate
    in if 2 <= l && l <= 14
       then if seats >= 2
            then Just (MkCar manufacturer licensePlate seats)
            else Nothing
        else Nothing

-}

makeCar :: String -> String -> Integer -> Validated Car
makeCar manufacturer licensePlate seats =
  let l = length licensePlate
   in if 2 <= l && l <= 14
        then
          if seats >= 2
            then Valid (MkCar manufacturer licensePlate seats)
            else Invalid ["not enough seats"]
        else
          if seats >= 2
          then Invalid ["license plate has wrong length"]
          else Invalid ["not enough seats", "license plate has wrong length"]
