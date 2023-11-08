module DB where

import qualified Data.Map as Map
import Data.Map (Map, (!))

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
      Get "Mike",
      Put "Mike" (x+1)]
-}

data DB a =
    Get Key       (Value -> DB a) -- "Callback" / "Continuation"
  | Put Key Value (()    -> DB a)
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
splice (Get key callback) next = 
    Get key (\value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)
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
    (>>=) = splice
    return = Return

p1'' :: DB String
p1'' = do put "Mike" 100
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show(x+y))

{-
in Java:
in Stream<A>:
<B> Stream<B> 	flatMap(Function<A, Stream<B>> mapper)
-}

-- >>> runDB Map.empty p1
-- ("201",fromList [("Mike",101)])
-- >>> runDB Map.empty p1''
-- ("201",fromList [("Mike",101)])
runDB :: Map Key Value -> DB a -> (a, Map Key Value)
runDB mp (Get key callback) = 
    runDB mp (callback (mp ! key))
runDB mp (Put key value callback) =
    runDB (Map.insert key value mp)
          (callback ())
runDB mp (Return result) = (result, mp)
