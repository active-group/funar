module Validation where

-- "Make illegal states unrepresentable." -- Yaron Minsky

data Car = MkCar { licensePlate :: String,
                   seatCount :: Integer }
    deriving Show

data Validated a =
    Valid a
  | Invalid [String]

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
