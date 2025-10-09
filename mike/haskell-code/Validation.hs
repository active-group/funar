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
    pure = Valid
    (<*>) :: Validated (a -> b) -> Validated a -> Validated b
    (<*>) (Valid f) (Valid a) = Valid (f a)
    (<*>) (Invalid ferrors) (Valid a) = Invalid ferrors
    (<*>) (Valid f) (Invalid aerrors) = Invalid aerrors
    (<*>) (Invalid ferrors) (Invalid aerrors) = Invalid (ferrors ++ aerrors)

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

-- (<*>) :: Validated (a -> b) -> Validated a -> Validated b

fmap2 :: (a -> b -> c) -> Validated a -> Validated b -> Validated c
-- fmap2 f va vb = ((pure f) <*> va) <*> vb
-- fmap2 f va vb = pure f <*> va <*> vb
-- fmap2 f va vb = fmap f va <*> vb
-- <$> = fmap
fmap2 f va vb = f <$> va <*> vb

fmap3 :: Applicative f => (a1 -> a2 -> a3 -> b) -> f a1 -> f a2 -> f a3 -> f b
fmap3 f va vb vc = f <$> va <*> vb <*> vc

-- >>> makeCar "VW" "TÜ GV256E" 5
-- Valid (MkCar {carManufacturer = MkManufacturer "VW", carLicensePlate = MkLicenseplate "T\220 GV256E", carSeats = MkSeatCount 5})

-- >>> makeCar "VW" "T" 1
-- Invalid ["wrong length for license plate","not enough seats"]
makeCar :: String -> String -> Integer -> Validated Car
makeCar  manufacturer licensePlate seats =
    fmap2 (MkCar (MkManufacturer manufacturer))
          (makeLicenseplate licensePlate)
          (makeSeatCount seats)
