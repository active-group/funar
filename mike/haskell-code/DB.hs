{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module DB where

import qualified Data.Map as Map
import Data.Map (Map, (!))

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

{-
put "Mike" 100
x = get "Mike"
put "Mike" (x+1)
y = get Mike
return (show (x+y))
-}

type Key = String
type Value = Integer

{-
-- Ablauf als Daten
data DBCommand result =
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [
    Put "Mike" 100,
    Get "Mike",
    Put "Mike" -- abhängig vom Ergebnis vom Get davor
]
-}

data DB a = -- a statt result
    Get Key       (Value -> DB a) -- Callback/Continuation
  | Put Key Value (()    -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show(x+y))))))

put :: Key -> Value -> DB ()
put key value = Put key value Return -- (\ value -> Return value)

get :: Key -> DB Value
get key = Get key Return

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next = 
    Get key (\value ->
        splice (callback value) next)
splice (Put key value callback) next = 
    Put key value (\() -> 
        splice (callback ())    next)
splice (Return result) next = next result

p1' :: DB String
p1' = splice (put "Mike" 100) (\() ->
      splice (get "Mike") (\x ->
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
      Return (show (x+y))))))

-- >>> runDB p1' Map.empty
-- ("201",fromList [("Mike",101)])

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

instance Functor DB where

instance Applicative DB where

instance Monad DB where
    (>>=) :: DB a -> (a -> DB b) -> DB b
    (>>=) = splice
    return :: a -> DB a
    return = Return

-- wichtig: alle Zeilen müssen genau untereinander anfangen
p1'' :: DB String
p1'' = do put "Mike" 100
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))

-- >>> runDB p1'' Map.empty
-- ("201",fromList [("Mike",101)])

runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key callback) mp = 
    let value = mp ! key
    in runDB (callback value) mp -- tail call
runDB (Put key value callback) mp =
    let mp' = Map.insert key value mp
    in runDB (callback ()) mp'
runDB (Return result) mp = (result, mp)

-- >>> runDB p1 Map.empty
-- ("201",fromList [("Mike",101)])

-- SQLite
-- CREATE TABLE IF NOT EXISTS entries (key TEXT PRIMARY KEY, value INTEGER)

execDB :: DB a -> IO a
execDB db =
    do connection <- open "test.sqlite"
       execute_ connection
         "CREATE TABLE IF NOT EXISTS entries (key TEXT PRIMARY KEY, value INTEGER)"
       result <- runDBSQLite db connection
       close connection
       return result

-- Datentyp für Zeile in der Tabelle
data Entry = MkEntry Key Value
  deriving Show

instance FromRow Entry where
    fromRow :: RowParser Entry
    fromRow = MkEntry <$> field <*> field

instance ToRow Entry where
    toRow :: Entry -> [SQLData]
    toRow (MkEntry key value) = toRow (key, value)

runDBSQLite :: DB a -> Connection -> IO a
runDBSQLite (Get key callback) connection =
    do [MkEntry foundKey value] <- 
          queryNamed connection "SELECT key, value FROM entries where key = :key" [":key" := key]
       runDBSQLite (callback value) connection
runDBSQLite (Put key value callback) connection = 
    do execute connection 
         "REPLACE INTO entries (key, value) VALUES (?,?)"
         (MkEntry key value)
       runDBSQLite (callback ()) connection
runDBSQLite (Return result) connection = return result

