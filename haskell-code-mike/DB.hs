{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs, DeriveFunctor, DeriveAnyClass #-}


module DB where

import qualified Data.Map as Map 
import Data.Map (Map)

import           Control.Applicative
import qualified Data.Text as T
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

-- getDB :: key -> value

-- DSL für Datenbankzugriffe

-- put "Mike" 15
-- x = get "Mike"
-- put "Mike" (x + 1)
-- return ("Mike ist " ++ show x)

{-
Erster Versuch:

type DB = [DBCommand]

data DBCommand =
    Put String Integer
  | Get String
-}

-- Datenbankprogramm mit Resultat vom Typ a
data DBCommand a =
    Put String Integer (() -> DBCommand a)
  | Get String (Integer -> DBCommand a) -- "Callback"
  | Done a

-- über die konkreten Aspekte von DBCommand
data Free f a = -- freie Monade
   Pure a -- Done
 | Impure (f (Free f a))
 deriving (Functor, Applicative)

{-
type DBCommand'' =
Free DBCommand' a =
    Pure a
  | Impure (DBCommand' (Free DBCommand' a))


-}

data DBCommand' self =
    Put' String Integer (() -> self)
  | Get' String (Integer -> self)

{-
data DBNetwork self =
     Put
   | Get
   
   | OpenSocket
   | Send
   | Receive
-}
type DBCommand'' a = Free DBCommand' a

instance Functor f => Monad (Free f) where
  return = Pure

  (Pure result) >>= cont = cont result
  (Impure f) >>= cont =
    Impure (fmap (>>= cont) f)

-- put :: String Integer DB -> DB

-- Lambda-Term:
-- \ x -> e
-- e1 e2 Beispiel: "f x"
-- x

-- \ x -> x x

p1 :: DBCommand String -- Datenbankprogramm, das eine Zeichenkette als Ergebnis liefert
p1 = Put "Mike" 15 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x + 1) (\() ->
     Done ("Mike ist " ++ show x))))

p1' = (put "Mike" 15) >>= (\() ->
      (get "Mike") `splice` (\x ->
      (put "Mike" (x + 1)) `splice` (\() ->
      return' ("Mike ist " ++ show x))))

-- kann *nur* auf die Datenbank zugreifen
put :: String -> Integer -> DBCommand ()
put key value = Put key value Done

get :: String -> DBCommand Integer
get key = Get key Done

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)

splice :: DBCommand a -> (a -> DBCommand b) -> DBCommand b
splice (Put key value cont) next =
  Put key value (\ () ->
    (cont ()) `splice` next)
splice (Get key cont) next =
  Get key (\ value ->
    (cont value) `splice` next)
splice (Done result) next = next result

-- Kleisli arrow
composeDBCommand :: (a -> DBCommand b) -> (b -> DBCommand c) -> (a -> DBCommand c)
composeDBCommand f g =
  \ a ->
    (f a) `splice` g

instance Functor DBCommand where
  -- fmap

instance Applicative DBCommand where

instance Monad DBCommand where
  (>>=) = splice
  return = Done

p1'' = do put "Mike" 15
          x <- get "Mike"
          put "Mike" (x + 1)
          return ("Mike ist " ++ show x)

return' :: a -> DBCommand a
return' = Done

evalDB :: Map String Integer -> DBCommand a -> a
evalDB db (Put key value cont) =
    evalDB (Map.insert key value db ) (cont ())
evalDB db (Get key cont) =
    case Map.lookup key db of
        Nothing -> undefined
        Just value ->
            evalDB db (cont value)
evalDB db (Done result) = result

-- DBCommand a -> IO a

data Entry = Entry String Integer

instance FromRow Entry where
  fromRow = Entry <$> field <*> field -- <$> == fmap

instance ToRow Entry where
  toRow (Entry key value) = toRow (key, value)

dbToSQLite :: Connection -> DBCommand a -> IO a
dbToSQLite connection (Put key value cont) =
  do execute connection "INSERT INTO test (key, value) VALUES (?, ?)" (Entry key value)
     dbToSQLite connection (cont ())
dbToSQLite connection (Get key cont) =
  do [Entry _ value] <- queryNamed connection "SELECT * from test WHERE key = :key" [":key" := key]
     dbToSQLite connection (cont value)
dbToSQLite connection (Done result) = return result

p2 = do put "Mike" 15
        x <- get "Mike"
        return x

execDB :: DBCommand a -> IO a
execDB command =
  do connection <- open ":memory:"
     execute_ connection "CREATE TABLE IF NOT EXISTS test (key TEXT PRIMARY KEY, value INTEGER)"
     result <- dbToSQLite connection command 
     close connection 
     return result

     


-- main :: IO ()