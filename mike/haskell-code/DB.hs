{-# LANGUAGE InstanceSigs #-}
module DB where

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

-- tut nicht:
p1 = [Put "Mike" 100,
      Put "Mike" (Get "Mike")]
-}
data DB result =
    Get Key       (Value -> DB result)
  | Put Key Value (()    -> DB result)
  | Return result

p1 :: DB String
p1 = Put "Mike" 100 (\() ->
     Get "Mike" (\x ->
     Put "Mike" (x+1) (\() ->
     Get "Mike" (\y ->
     Return (show (x+y))))))

get :: Key -> DB Value
-- get key = Get key (\value -> Return value)
get key = Get key Return

put :: Key -> Value -> DB ()
put key value = Put key value Return

p1' :: DB String
p1' = splice (put "Mike" 100) (\() ->
      splice (get "Mike") (\x ->
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
              Return (show (x+y))))))             

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key
      (\ value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value
      ( \()    -> splice (callback ()) next)
splice (Return result) next = next result

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   -- "bind"
--   (>>=) :: m a -> (a -> m b) -> m b
--   return :: a -> m a

instance Functor DB where

instance Applicative DB where
    
instance Monad DB where
    (>>=) :: DB a -> (a -> DB b) -> DB b
    (>>=) = splice
    return = Return

-- syntaktischer Zucker für p1'
p1''' :: DB String
p1''' = do put "Mike" 100
           x <- get "Mike"
           put "Mike" (x+1)
           y <- get "Mike"
           return (show (x+y))