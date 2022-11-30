{-# LANGUAGE OverloadedStrings #-}
module DB where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import qualified Data.Map as Map 
import Data.Map (Map, (!))

{-
put "Mike" 51
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

type Key = String
type Value = Integer

{-
data DBCommand a = -- a: Typ des Ergebnisses 
    Put Key Value
  | Get Key
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 51,
      Get "Mike"]
-}

data DB a =
    Get Key (Value -> DB a)
  | Put Key Value (() -> DB a) -- (): "unit", Wert auch ()
  | Return a

-- Get/Put unabhängig machen, später zusammensetzen
get :: Key -> DB Value
get key = Get key Return -- (\value -> Return value)

put :: Key -> Value -> DB ()
put key value = Put key value Return -- (\() -> Return ())

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next = 
    Get key (\value -> 
        splice (callback value) next)
splice (Put key value callback) next = 
    Put key value (\() ->
        splice (callback ()) next)
splice (Return result) next = next result

p1 :: DB String
p1 = Put "Mike" 51 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

p1' :: DB String
p1' = put "Mike" 51 `splice` (\() ->
      get "Mike" `splice` (\x ->
      put "Mike" (x+1) `splice` (\() ->
      get "Mike" `splice` (\y ->
      Return (show (x+y))))))

p1'' :: DB String
-- >>> runDB Map.empty p1''
-- ("103",fromList [("Mike",52)])
p1'' = do put "Mike" 51
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))
-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

instance Functor DB where

-- >>> :info Functor
-- type Functor :: (* -> *) -> Constraint
-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b

-- map :: (a -> b) -> [a] -> [b]

-- instance Functor [] where ...

instance Applicative DB where

instance Monad DB where
    (>>=) = splice
    return = Return

runDB :: Map Key Value -> DB a -> (a, Map Key Value)
-- >>> runDB Map.empty p1
-- ("103",fromList [("Mike",52)])
runDB map (Get key callback) = 
    let value = map ! key
    in runDB map (callback value)
runDB map (Put key value callback) = 
    let map' = Map.insert key value map
    in runDB map' (callback ())
runDB map (Return result) = (result , map)

-- Tabelle entries, 2 Spalten key, value

-- entspricht einer Zeile in der Datenbank
data Entry = MkEntry Key Value

instance FromRow Entry where
    fromRow = MkEntry <$> field <*> field

instance ToRow Entry where
    toRow (MkEntry key value) = toRow (key, value)

runDBSQLite :: Connection -> DB a -> IO a
runDBSQLite conn (Get key callback) = 
    do [MkEntry _ value] 
        <- queryNamed conn "SELECT key, value FROM entries WHERE key = :key" [":key" := key]
       runDBSQLite conn (callback value)
runDBSQLite conn (Put key value callback) = 
    do execute conn "REPLACE INTO entries (key, value) VALUES (?,?)" (MkEntry key value)
       runDBSQLite conn (callback ())
runDBSQLite conn (Return result) = return result

execDBSQLite :: DB a -> IO a
execDBSQLite db =
    do conn <- open "test.db"
       execute_ conn#
        "CREATE TABLE IF NOT EXISTS entries (key TEXT PRIMARY KEY, value INTEGER)"