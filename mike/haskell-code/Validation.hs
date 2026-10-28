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

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

--   fmap  ::   (a -> b) -> f a -> f b


-- data Maybe a = Nothing | Just a
mkCar :: String -> Integer -> Validated Car

-- >>> mkCar "D" 4
-- Invalid ["wrong length of license plate"]
-- >>> mkCar "D-2412" 4
-- Valid (MkCar {licensePlate = "D-2412", seatCount = 4})
-- >>> mkCar "D" 1
-- Invalid ["wrong length of license plate","not enough seats"]
mkCar l n = validatedMap2 MkCar (validateLicensePlate l) (validateSeatCount n)

{-
    if n < 2
    then if length l <= 2 || length l > 14
         then Invalid ["not enough seats", "wrong length of license plate"]
         else Invalid ["not enough seats"]
    else if length l <= 2 || length l > 14
    then Invalid ["wrong length of license plate"]
    else Valid (MkCar l n)
-}

validatedMap2 :: (a -> b -> c) -> Validated a -> Validated b -> Validated c
validatedMap2 f (Valid a) (Valid b) = Valid (f a b)
validatedMap2 f (Invalid errors) (Valid b) = Invalid errors
validatedMap2 f (Invalid errorsa) (Invalid errorsb) = Invalid (errorsa ++ errorsb)
validatedMap2 f (Valid a) (Invalid errors) = Invalid errors

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
