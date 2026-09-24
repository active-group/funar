module Validation where

-- "Make illegal states unpresentable." -- Yaron Minsky

data SimpleMovie = MkSimpleMovie { simpleTitle :: String }
  deriving Show

checkTitle :: String -> Maybe String
checkTitle s = if length s == 0
               then Nothing
               else Just s

mkSimpleMovie :: String -> Maybe SimpleMovie
-- mkSimpleMovie s = MkSimpleMovie (checkTitle s)
mkSimpleMovie s = fmap MkSimpleMovie (checkTitle s)

data Movie = MkMovie { title :: String,
                       price :: Integer,
                       rating :: String }
  deriving Show

maybeMap2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
maybeMap2 f Nothing _ = Nothing
maybeMap2 f _ Nothing = Nothing
maybeMap2 f (Just a) (Just b) = Just (f a b)

-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

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
