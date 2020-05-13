module Zipper where

import Data.Maybe (listToMaybe)

import Prelude hiding (length)
import qualified Prelude as Prelude

data Zipper a =
  Zip { reverseFront :: [a], back :: [a] }
  deriving (Show, Eq, Ord)

instance Functor Zipper  where
  fmap f (Zip reverseFront back) = Zip (map f reverseFront) (map f back)

-- exercise: Foldable

fromList :: [a] -> Zipper a
fromList as = Zip [] as

toList :: Zipper a -> [a]
toList (Zip reverseFront back) = reverse reverseFront ++ back

cursor :: Zipper a -> a
cursor (Zip _ (a:_)) = a

safeCursor :: Zipper a -> Maybe a
safeCursor (Zip _ back) = listToMaybe back

left :: Zipper a -> Zipper a
left  (Zip (a:reverseFront) back) = Zip reverseFront (a:back)
left  zipper = zipper

safeLeft :: Zipper a -> Maybe (Zipper a)
safeLeft  (Zip (a:reverseFront) back) = Just (Zip reverseFront (a:back))
safeLeft  _ = Nothing

right :: Zipper a -> Zipper a
right (Zip reverseFront (a:back)) = Zip (a:reverseFront) back
right zipper = zipper

safeRight :: Zipper a -> Maybe (Zipper a)
safeRight (Zip reverseFront (a:back)) = Just (Zip (a:reverseFront) back)
safeRight _ = Nothing

length :: Zipper a -> Int
length (Zip reverseFront back) = (Prelude.length reverseFront) + (Prelude.length back)

