
{-# LANGUAGE KindSignatures #-}
module List where

listSum :: [Integer] -> Integer
listSum [] = 0
listSum (x:xs) = x + (listSum xs)

f [x, y, z] = x + y + z

listFold :: t1 -> (t2 -> t1 -> t1) -> [t2] -> t1
listFold forEmpty forCons []     = forEmpty
listFold forEmpty forCons (x:xs) =
    x `forCons` (listFold forEmpty forCons xs)

-- natürlichen Zahlen ab ... als Liste liefern
natsFrom n = n : (natsFrom (n + 1))

nats = natsFrom 0

-- Vielfache einer Zahl aus einer Liste streichen
strikeMultiples :: Integer -> [Integer] -> [Integer]
strikeMultiples n [] = []
strikeMultiples n (x:xs)
    | x `mod` n == 0 = strikeMultiples n xs
    | otherwise = x : (strikeMultiples n xs)

sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (x:xs) = x : sieve (strikeMultiples x xs)

primes = sieve (natsFrom 2)

data InfiniteList a =
  ICons a (() -> InfiniteList a)

-- Assoziativität:
-- (a + b) + c = a + (b + c)

-- Halbgruppe:
-- Menge M
-- o :: M -> M -> M
-- (a `o` b) `o` c = a `o` (b `o` c)
-- "semigroup"
class MSemigroup (a :: *) where
    -- schön wäre: Assoziativität als Code
    o :: a -> a -> a

instance MSemigroup Integer where
    o = (+)

-- wie data, aber nur für 1 einstelligen Konstruktor
-- keine Laufzeitrepräsentation
newtype Additive = Additive Integer
newtype Multiplicative = Multiplicative Integer

instance MSemigroup Additive where 
    (Additive i1) `o` (Additive i2) = Additive (i1 + i2)

instance MSemigroup [a] where 
    o lis1 lis2 = lis1 ++ lis2 -- "concat"

-- "damit (a, b) eine Halbgruppe bildet, müssen a und b auch jeweils eine bilden"
instance (MSemigroup a, MSemigroup b) => MSemigroup (a, b) where
    o (a1, b1) (a2, b2) = (a1 `o` a2, b1 `o` b2) -- 3 os!

-- Monoid
-- Halbgruppe + neutrales Element
-- n :: M
-- n `o` x == x `o` n == x

class MSemigroup a => MMonoid a where 
   -- n `o` x == x `o` n == x
   n :: a

-- Maybe :: * -> *
-- Integer :: *
maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap f Nothing  = Nothing 
maybeMap f (Just x) = Just (f x)

class MFunctor (m :: * -> *) where
    mmap :: (a -> b) -> m a -> m b