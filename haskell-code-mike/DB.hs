module DB where

import Data.Map.Strict as Map 
import Data.Map.Strict (Map, (!))

{-
  map1 = put Map.empty "Mike" 10
  x = get map1 "Mike"
  map2 = put map2 "Mike" (x + 1)
-}
{-
   put "Mike" 10
   x = get "Mike"
   put "Mike" (x + 1)
   y = get "Mike"
-}

type Key = String
type Value = Integer

{-
data DB 
  = Put Key Value
  | Get Key

type DBProgram = [DB]

p = [Put "Mike" 10, Get "Mike", Put "Mike" (x + 1)]
-}

data DB result =
    Get Key (Value -> DB result)
  | Put Key Value (() -> DB result)
  | Done result


p :: DB Value
p = Put "Mike" 10
      (\ () ->
        Get "Mike" 
          (\ x ->
            Put "Mike" (x + 1) 
               (\ () ->
                 Get "Mike" 
                   (\ y -> Done y))))


run :: Map Key Value -> DB result -> result
run db (Get key callback) =
    let value = db ! key
    in run db (callback value) -- Typ DB result
run db (Put key value callback) =
    let db' = Map.insert key value db
    in run db' (callback ())
run _db (Done result) = result

get :: Key -> DB Value
get key = Get key (\ result -> Done result)

put :: Key -> Value -> DB ()
put key value = Put key value (\ () -> Done ())

-- Zwei DB-Programme hintereinander
splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\ value -> splice (callback value) next)
splice (Put key value callback) next = 
    Put key value (\ () -> splice (callback ()) next)
splice (Done result) next = next result


p' = put "Mike" 10 `splice` (\ () ->
     get "Mike" `splice` (\ x ->
     put "Mike" (x + 1) `splice` (\ () ->
     get "Mike" `splice` (\ y ->
     Done y))))

instance Functor DB where

instance Applicative DB where

instance Monad DB where
   (>>=) = splice
   return x = Done x

p'' :: DB Value
p'' = do put "Mike" 10
         x <- get "Mike"
         put "Mike" (x + 1)
         y <- get "Mike"
         return y

q = do get "Mike"