{-# LANGUAGE InstanceSigs #-}
module Validation where

-- "Make illegal states unrepresentable." -- Yaron Minsky

data UserName = MkUserName String
  deriving Show
type UserAge = Integer

data User = MkUser UserName UserAge
  deriving Show


-- smart constructor
{-
mkUser :: String -> Integer -> User
mkUser userName userAge =
    if userName == ""
    then error "username empty"
    else if userAge < 0 || userAge > 125
    then error "age invalid"
    else MkUser userName userAge

-- >>> mkUser "" 140
-- username empty
-}

data Validation a =
    Valid a 
  | Invalid [String]
  deriving Show

instance Functor Validation where
    fmap :: (a -> b) -> Validation a -> Validation b
    fmap f (Valid a) = Valid (f a)
    fmap f (Invalid errors) = Invalid errors
-- >>> :info Applicative
-- type Applicative :: (* -> *) -> Constraint
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b

-- fmap ::       (a ->   b) -> f a -> f b
-- (<*>)::     f (a ->   b) -> f a -> f b
-- flip (>>=) :: (a -> f b) -> f a -> f b


instance Applicative Validation where
    pure :: a -> Validation a
    pure a = Valid a
    (<*>) :: Validation (a -> b) -> Validation a -> Validation b
    (<*>) (Invalid errors1) (Invalid errors2) = Invalid (errors1++errors2)
    (<*>) (Invalid errors) (Valid a) = Invalid errors
    (<*>) (Valid fab) (Invalid errors) = Invalid errors
    (<*>) (Valid fab) (Valid a) = Valid (fab a)

-- Java-/Spring-Praxis:
-- validateXXX :: XXXDto -> Validation XXX

-- "parse, don't validate"
validateName :: String -> Validation UserName
validateName userName =
    if userName == ""
    then Invalid ["username empty"]
    else Valid (MkUserName userName)

validateAge :: Integer -> Validation Integer
validateAge userAge =
    if userAge < 0 || userAge > 125
    then Invalid ["invalid age"]
    else Valid userAge

-- vmap2 :: (a -> b -> c) -> Validation a -> Validation b -> Validation c
vmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
vmap2 fab va vb =
    fab <$> va <*> vb

-- (<$>) = fmap

vmap3 :: Applicative f => (a1 -> a2 -> a3 -> b) -> f a1 -> f a2 -> f a3 -> f b
vmap3 f va vb vc =
    pure f <*> va <*> vb <*> vc

mkUser :: String -> Integer -> Validation User
mkUser userName userAge =
    MkUser <$> validateName userName <*> validateAge userAge
{-    if userName == ""
    then Invalid ["username empty"]
    else if userAge < 0 || userAge > 125
    then Invalid ["invalid age"]
    else Valid (MkUser userName userAge)
-}

-- >>> mkUser "Mike" 53
-- Valid (MkUser "Mike" 53)

-- >>> mkUser "" 53
-- Invalid ["username empty"]

-- >>> mkUser "" 150
-- Invalid ["username empty","invalid age"]
