module DB where

import qualified Data.Map.Strict as Map 
import Data.Map (Map, (!))

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

{-
put "Mike" 50
x = get "Mike"
put "Mike" (x+1)
y = get "Mike"
return (show (x + y))
-}

{-
data DBCommand a =
    Put String Integer 
  | Get String 
  | Return a

type DBProgram a = [DBCommand a]

p1 = [Put "Mike" 50, 
      Get "Mike"]
-}

data DB a =
    Get String         (Integer -> DB a) -- "callback" / "continuation"
  | Put String Integer (()      -> DB a)
  | Return a

p1 :: DB String
p1 = Put "Mike" 50 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x + y))))))

get :: String -> DB Integer 
get key = Get key Return -- (\ value -> Return value)

put :: String -> Integer -> DB ()
put key value = Put key value Return -- (\() -> Return ())

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key cont) next =
    Get key (\ value ->
               splice (cont value) next)
splice (Put key value cont) next =
    Put key value (\ () ->
                    splice (cont ()) next)
splice (Return result) next = next result

p1' :: DB String
p1' = put "Mike" 50 `splice` (\() ->
      get "Mike" `splice` (\x -> 
      put "Mike" (x+1) `splice` (\() ->
      get "Mike" `splice` (\y ->
      Return (show (x+y))))))

p1''' :: DB String
p1''' =
  put "Mike" 50 >>= ( \() ->
  get "Mike">>= ( \x ->
  put "Mike" (x + 1) >>= ( \() ->
  get "Mike" >>= ( \y ->
  return (show (x + y))))))
-- >>> runDB p1 Map.empty
-- "101"
-- >>> runDB p1' Map.empty
-- "101"
-- >>> runDB p1'' Map.empty
-- "101"
runDB :: DB a -> Map String Integer -> a
runDB (Get key cont) db = 
    runDB (cont (db ! key)) db
runDB (Put key value cont) db = 
    runDB (cont ()) (Map.insert key value db)
runDB (Return result) db = result

instance Functor DB where

instance Applicative DB where

instance Monad DB where
    (>>=) = splice
    return = Return

p1'' :: DB String
p1'' = do put "Mike" 50
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))

-- IO ... 