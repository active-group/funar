{-# LANGUAGE InstanceSigs #-}
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

-- vereinfacht:
-- Validated SeatCount -> Validated Car


makeSeatCount :: Integer -> Validated SeatCount
makeSeatCount n =
    if n >= 2
    then Valid (MkSeatCount n)
    else Invalid ["not enough seats"]

-- if-Kaskade
-- keine Fehlermeldung

data Validated a =
    Valid a
  | Invalid [String] -- Fehlermeldung
  deriving Show

instance Functor Validated where
    fmap :: (a -> b) -> Validated a -> Validated b
    fmap f (Invalid errors) = Invalid errors
    fmap f (Valid a) = Valid (f a)

-- applikativer Funktor
-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

instance Applicative Validated where
    pure :: a -> Validated a
    (<*>) :: Validated (a -> b) -> Validated a -> Validated b

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

{-
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
-}

{-
makeCar :: String -> String -> Integer -> Validated Car
makeCar manufacturer licensePlate seats =
    case makeLicenseplate licensePlate of
        Valid licensePlate ->
            case makeSeatCount seats of
                Valid seatCount ->
                    Valid (MkCar (MkManufacturer manufacturer) licensePlate seatCount)
                -- ...

-}

fmap2 :: (a -> b -> c) -> Validated a -> Validated b -> Validated c
fmap2 = undefined

makeCar :: String -> String -> Integer -> Validated Car
makeCar  manufacturer licensePlate seats =
    fmap2 (MkCar (MkManufacturer manufacturer))
          (makeLicenseplate licensePlate)
          (makeSeatCount seats)
