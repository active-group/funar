{-# LANGUAGE InstanceSigs #-}
module Validation where

-- "Make illegal states unrepresentable." -- Yaron Minsky

data Car = MkCar { licensePlate :: String,
                   seatCount :: Integer }
    deriving Show

data Validated a =
    Valid a
  | Invalid [String]
  deriving Show

instance Functor Validated where
    fmap :: (a -> b) -> Validated a -> Validated b
    fmap f (Valid a) = Valid (f a)
    fmap f (Invalid errors) = Invalid errors

-- data Maybe a = Nothing | Just a
mkCar :: String -> Integer -> Validated Car
mkCar l n =
    if n < 2
    then if length l <= 2 || length l > 14
         then Invalid ["not enough seats", "wrong length of license plate"]
         else Invalid ["not enough seats"]
    else if length l <= 2 || length l > 14
    then Invalid ["wrong length of license plate"]
    else Valid (MkCar l n)

data Car1 = MkCar1 { licensePlate1 :: String }
  deriving Show

validateLicensePlate :: String -> Validated String
validateLicensePlate l =
    if length l <= 2 || length l > 14
    then Invalid ["wrong length of license plate"]
    else Valid l

mkCar1 :: String -> Validated Car1
mkCar1 l = fmap MkCar1 (validateLicensePlate l)

validateSeatCount :: Integer -> Validated Integer
validateSeatCount n =
    if n < 2
    then Invalid ["not enough seats"]
    else Valid n