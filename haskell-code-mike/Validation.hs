module Validation where

import Data.Char

data Person = Person {
    personName :: String,
    personEmail :: String,
    personAge :: Int
}

p1 = Person "Mike" "sperber@deinprogramm.de" 49
p2 = Person "Fritz" "fritz/-/(&/(" 500


-- Typsystem nicht stark genug, um illegale Daten zu verbieten

-- Funktionale Programmierung: "make illegal states unrepresentable"
-- schwache Version: "don't create illegal states"

-- "parse, don't validate"

{-
-- Fehlschlag 0:
validateAge age = if age <= 120
                  then Just age
                  else Nothing -- keine Fehlermeldung

-- Fehlschlag 1: (kein Parsing)
validateAge' age = if age <= 120
                   then Nothing -- kein Parsing
                   else Just "too old"
-}

-- Validation :: Type -> (Type -> Type)
data Validation error result =
    Success result
  | Failure [error]
  deriving Show

validateAge age = if age <= 120
                  then Success age
                  else Failure ["too old"]

validateName name = Success name

validateEmail email = if elem '@' email 
                      then Success (map toLower email)
                      else Failure ["no at sign"]

-- Problem: Komposition

{-
class Functor (f :: Type -> Type) where
    fmap :: (a -> b) -> f a -> f b
-}

-- akzeptiert noch Typvariable result
instance Functor (Validation error) where
    -- fmap :: (a - > b) -> Validation error a -> Validation error b
    fmap f (Success a) = Success (f a)
    fmap f (Failure errors) = Failure errors

-- nur E-Mail!
data Person' = Person' String
  deriving Show

validatePerson' :: String -> Validation String Person'
validatePerson' email = fmap Person' (validateEmail email)

-- Funktor nicht stark genug für mehrere Attribute!
-- Idee: 
-- applicate :: f (a -> b) -> f a -> f b
-- applikativer Funktor / "applicative"
-- braucht außerdem Funktion pure :: a -> f a

-- (>>=) :: m a -> (a -> m b) -> m b

applicate :: Validation error (a -> b) -> Validation error a -> Validation error b 
applicate (Failure errors1) (Failure errors2) =
    Failure (errors1 ++ errors2)
applicate (Failure errors) success = Failure errors
applicate success (Failure errors) = Failure errors
applicate (Success f) (Success a) =
    Success (f a)

validatePerson name email age =
    -- applicate (applicate (fmap Person (validateName name)) (validateEmail email)) (validateAge age)
--    Person `fmap` (validateName name) `applicate` (validateEmail email) `applicate` (validateAge age)
    Person <$> (validateName name) <*> (validateEmail email) <*> (validateAge age)
    -- Person name email age
    -- "applicative style"

instance Applicative (Validation error) where
    pure = Success
    (<*>) = applicate

-- außerdem ist <$> ein Synonym für fmap

-- Haxl:


{-
validatePerson :: String -> String -> Int -> Validation String Person
validatePerson name email age =
    case validateName name of
        Success name ->
            case validateEmail email of
                Success email ->
                    case validateAge age of
                        Success age ->
                            Success (Person name email age)
                        Failure errors -> Failure errors
                Failure errors ->
                    case validateAge age of
                        Success age -> Failure errors
                        Failure errors' -> Failure (errors ++ errors')
 -- ...

 data PersonDraft = PersonDraft {
    personDraftName :: Validation String String,


-}