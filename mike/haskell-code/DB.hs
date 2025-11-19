{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Control.Applicative
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

{-
put "Mike" 100
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x+y))
-}

type Key = String

type Value = Integer

{-
data DBCommand result = 
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [Put "Mike" 100,
      Get "Mike",
      Put "Mike" (x+1)]
-}

data DB a =
    Get Key (Value -> DB a)
  | Put Key Value (() -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

get :: Key -> DB Value
get key = Get key Return -- (\value -> Return value)

put :: Key -> Value -> DB ()
put key value = Put key value Return

splice :: DB a -> (a -> DB b) -> DB b
splice (Return a) next = next a
splice (Get key callback) next =
    Get key (\value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)

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
    -- "bind"
    (>>=) :: DB a -> (a -> DB b) -> DB b
    (>>=) = splice
    return :: a -> DB a
    return = Return

p1'' :: DB String
p1'' =
    do put "Mike" 100
       x <- get "Mike"
       put "Mike" (x+1)
       y <- get "Mike"
       return (show (x+y))

-- >>> runDB p1 Map.empty
-- ("201",fromList [("Mike",101)])

runDB :: DB a -> Map Key Value -> (a, Map Key Value)
runDB (Return result) db = (result, db)
runDB (Get key callback) db =
    runDB (callback (db ! key)) db
runDB (Put key value callback) db = 
    let db' = Map.insert key value db
    in runDB (callback ()) db'

-- fmap ::       (a ->   b) -> f a -> f b
-- (<*>) ::    f (a ->   b) -> f a -> f b
-- flip (>>=) :: (a -> f b) -> f a -> f b

-- (>>=) :: f a -> (a -> f b) -> f b

data Entry = MkEntry String Integer

instance FromRow Entry where
    fromRow :: RowParser Entry
    fromRow = MkEntry <$> field <*> field

instance ToRow Entry where
    toRow :: Entry -> [SQLData]
    toRow (MkEntry key value) = toRow (key, value)

runDBSQLite :: DB a -> Connection -> IO a
runDBSQLite (Return a) conn = return a
runDBSQLite (Get key callback) conn = 
    do [MkEntry key' value] <- queryNamed conn "SELECT key, value FROM entries WHERE key = :key" [":key" := key]
       runDBSQLite (callback value) conn
runDBSQLite (Put key value callback) conn =
    do execute conn "REPLACE INTO entries (key,value) VALUES (?, ?)" (MkEntry key value)
       runDBSQLite (callback ()) conn

-- >>> execDB p1''
-- "201"

execDB :: DB a -> IO a
execDB program = do conn <- open "test.db"
                    execute_ conn "CREATE TABLE IF NOT EXISTS entries (key TEXT PRIMARY KEY, value INTEGER)"
                    result <- runDBSQLite program conn
                    close conn
                    return result

-- CREATE TABLE entries (key TEXT PRIMARY KEY, value INTEGER)

-- class Stream<A> {
--    <B> Stream<B> flatMap(Function<A, Stream<B>> mapper)
-- }