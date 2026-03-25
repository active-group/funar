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
    Sucess a
  | Failure [String] -- Fehlermeldungen

mkSeatCount :: Integer -> Maybe SeatCount
mkSeatCount n =
    if n >= 2
    then Just (MkSeatCount n)
    else Nothing

mkLicensePlate :: String -> Maybe LicensePlate
mkLicensePlate s =
    let l = length s
    in if l >= 2 && l <= 14
       then Just (MkLicensePlate s)
       else Nothing

mkCar :: String -> Integer -> Maybe Car
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
        (Just licensePlate, Just seatCount) ->
            Just (MkCar licensePlate seatCount)
        _ -> Nothing

-- Wunschliste:
-- - Fehlermeldungen
-- - n-stelliges fmap