{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

import Control.Applicative
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
data DBCommand a = 
    Put Key Value
  | Get Key
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 100,
      Get "Mike"]
-}
data DB a =
    Get Key       (Value -> DB a) -- Callback / Continuation
  | Put Key Value (()    -> DB a)
  | Return a

get :: Key -> DB Value
get key = Get key Return -- (\value -> Return value)

put :: Key -> Value -> DB ()
put key value = Put key value Return

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)
splice (Return result) next = next result

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

-- fmap       ::    (a ->  b) -> f a -> f b
-- (<*>)      :: f (a ->   b) -> f a -> f b
-- flip (>>=) ::   (a -> f b) -> f a -> f b


instance Functor DB where

instance Applicative DB where

instance Monad DB where
    return :: a -> DB a
    return = Return
    (>>=) :: DB a -> (a -> DB b) -> DB b
    (>>=) = splice

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

p1' :: DB String
p1' = splice (put "Mike" 100) (\() ->
      splice (get "Mike") (\ x ->
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\ y -> 
      Return (show (x+y))))))
      

p1'' :: DB String
p1'' = do put "Mike" 100
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))

-- DB-Programm ausführen
executeDB :: DB a -> Map Key Value -> (a, Map Key Value)

-- >>> executeDB p1'' Map.empty
-- ("201",fromList [("Mike",101)])

executeDB (Get key callback) mp = 
    let value = mp ! key
    in executeDB (callback value) mp
executeDB (Put key value callback) mp = 
    let updated = Map.insert key value mp
    in executeDB (callback ()) updated
executeDB (Return result) mp = (result, mp)

data Entry = MkEntry Key Value
  deriving Show

instance FromRow Entry where
    fromRow :: RowParser Entry
    fromRow = MkEntry <$> field <*> field

instance ToRow Entry where
    toRow :: Entry -> [SQLData]
    toRow (MkEntry key value) = toRow (key, value)

executeDBSqLite :: DB a -> Connection-> IO a
executeDBSqLite (Get key callback) conn = 
    do [MkEntry key' value] <- queryNamed conn "SELECT key, value FROM entries WHERE key=:key"
                                 [":key" := key]
       executeDBSqLite (callback value) conn
executeDBSqLite (Put key value callback) conn =
    do execute conn "REPLACE INTO entries (key, value) VALUES (?, ?)" 
        (MkEntry key value)
       executeDBSqLite (callback ()) conn
executeDBSqLite (Return result) conn = return result

{-
CREATE TABLE entries (key TEXT PRIMARY, value INTEGER)
-}

runDBSqLite :: DB a -> IO a
runDBSqLite db =
    do conn <- open "test.db"
       execute_ conn "CREATE TABLE IF NOT EXISTS entries (key TEXT PRIMARY, value INTEGER)"
       result <- executeDBSqLite db conn
       close conn
       return result