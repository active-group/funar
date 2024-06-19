{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module DB where

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))
{-
put "Mike" 100
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

type Key = String

type Value = Integer

{- 
-- gescheitert:

data DBCommand a = -- a: Typ des Ergebnisses, von return
    Put Key Value
  | Get Key 
  | Return a
  deriving Show

type DBProgram a = [DBCommand a]

p1 = [
    Put "Mike" 100,
    Get "Mike",
    Put "Mike" ???
]
-}

-- Namen vergeben: let, =, \

data DB a =
    Get Key       (Value -> DB a) -- callback / continuation
  | Put Key Value (()    -> DB a)
-- | Transaction (DB (DB a))
  | Return a

p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x -> 
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

-- - Notation schicker
-- - laufen lassen

-- 1. Idee: Programm zerlegen

-- DB a: (Beschreibung eines) Programms, das mit der Datenbank redet
--       und ein Ergebnis vom Typ a liefert

get :: Key -> DB Value
get key = Get key Return -- (\value -> Return value)

put :: Key -> Value -> DB ()
put key value = Put key value Return -- (\() -> Return ())

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key cont) next =
    Get key       (\value -> splice (cont value) next)
splice (Put key value cont) next =
    Put key value (\() -> splice (cont ()) next)
splice (Return result) next = next result

p1' :: DB String
p1' = splice (put "Mike" 100) (\() ->
      splice (get "Mike") (\x -> 
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
      Return (show (x+y))))))

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

p1'' :: DB String
p1'' = do put "Mike" 100
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))

-- >>> runDB p1'' Map.empty
-- ("201",fromList [("Mike",101)])

runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Get key cont) mp =
    let value = mp ! key 
    in runDB (cont value) mp
runDB (Put key value cont) mp =
    let mp' = Map.insert key value mp
    in runDB (cont ()) mp'
runDB (Return result) mp = (result, mp)

-- IO: IO-Monade
-- main :: IO ()
-- benötigen Datentyp, der den Datenbank-Zeilen entspricht
data Entry = MkEntry Key Value

instance FromRow Entry where
  fromRow = MkEntry <$> field <*> field

instance ToRow Entry where
  -- toRow :: Entry -> [SQLData]
  toRow (MkEntry key value) = toRow (key, value)

runDBSQLite :: Connection -> DB a -> IO a
runDBSQLite conn (Get key callback) =
  do
    [entry] <-
      -- OverloadedStrings macht aus SQL-Text ein Query-Objekt
      queryNamed conn "SELECT key, value FROM entries WHERE key = :key" [":key" := key]
    let (MkEntry _ value) = entry
    runDBSQLite conn (callback value)
runDBSQLite conn (Put key value callback) =
  do
    execute conn "REPLACE INTO entries (key, value) VALUES (?,?)" (MkEntry key value)
    runDBSQLite conn (callback ())
runDBSQLite conn (Return result) = return result

-- >>> execDB p1
-- "103"
execDB :: DB a -> IO a
execDB db =
  do
    conn <- open "test.db"
    execute_
      conn
      "CREATE TABLE IF NOT EXISTS entries (key TEXT PRIMARY KEY, value INTEGER)"
    result <- runDBSQLite conn db
    close conn
    return result
