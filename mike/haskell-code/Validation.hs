module Validation where

data Validation a =
    Sucess a
  | Failure [String]
  deriving Show

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

maybeMap2 :: (a -> (b -> c)) -> Maybe a -> Maybe b -> Maybe c
-- maybeMap2 f Nothing _ = Nothing
-- maybeMap2 f _ Nothing = Nothing
-- maybeMap2 f (Just a) (Just b) = Just (f a b)
-- maybeMap2 :: Applicative f => (a -> b) -> f a -> p -> f b
-- maybeMap2 f ma mb = pure f <*> ma <*> mb
maybeMap2 f ma mb = f <$> ma <*> mb

-- (<$>) = fmap

-- pure f <*> ma :: f (b -> c)

maybeMap3 :: (a -> b -> c -> d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
maybeMap3 f ma mb mc = pure f <*> ma <*> mb <*> mc

-- n-ary fmap:
-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

--      fmap :: (a -> b) -> f a -> f b

-- values of this type should always be valid

checkPrice :: Integer -> Maybe Integer
checkPrice p = if p < 1 || p > 100
               then Nothing
               else Just p

checkRating :: String -> Maybe String
checkRating s = if length s /= 5
                then Nothing
                else Just s

mkMovie :: String -> Integer -> String -> Maybe Movie
mkMovie title price rating =
--   "MkMovie (checkTitle title) (checkPrice price) (checkRating rating)"
   MkMovie <$> checkTitle title <*> checkPrice price <*> checkRating rating

{-
    if length title == 0
    then Nothing
    else if price < 1 || price > 100
         then Nothing
         else if length rating /= 5
              then Nothing
              else Just (MkMovie title price rating)
-}