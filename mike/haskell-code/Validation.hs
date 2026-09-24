module Validation where

-- "Make illegal states unpresentable." -- Yaron Minsky

data Movie = MkMovie { title :: String,
                       price :: Integer,
                       rating :: String }
  deriving Show

-- values of this type should always be valid

mkMovie :: String -> Integer -> String -> Maybe Movie
mkMovie title price rating =
    if length title == 0
    then Nothing
    else if price < 1 || price > 100
         then Nothing
         else if length rating /= 5
              then Nothing
              else Just (MkMovie title price rating)