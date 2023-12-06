module DB where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map, (!))

type Key = String
type Value = Integer

{-
put "Mike" 100
x = get "Mike" 
put "Mike" (x+1)
y = get "Mike"
return (show (x+y))
-}

{-
data DBCommand result =
    Put Key Value
  | Get Key
  | Return result

type DBProgram result = [DBCommand result]

p1 = [Put "Mike" 100,
      Get "Mike",
      Put "Mike"] -- wie dem Resultat vom Get einen Namen geben?
-}

data DB a =
    Get Key       (Value -> DB a)
  | Put Key Value (()    -> DB a)
  | Return a

p1 :: DB String
p1 =
    Put "Mike" 100 (\() ->
    Get "Mike" (\x ->
    Put "Mike" (x+1) (\() ->
    Get "Mike" (\y -> 
    Return (show (x+y))))))

put :: Key -> Value -> DB ()
put key value = Put key value (\() -> Return ())

get :: Key -> DB Value
get key = Get key (\value -> Return value)

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key callback) next =
    Get key (\ value -> splice (callback value) next)
splice (Put key value callback) next =
    Put key value (\() -> splice (callback ()) next)
splice (Return result) next = next result

p1' = splice (put "Mike" 100) (\() ->
      splice (get "Mike") (\x ->
      splice (put "Mike" (x+1)) (\() ->
      splice (get "Mike") (\y ->
      Return (show (x+y))))))

-- >>> runDB p1' Map.empty
-- ("201",fromList [("Mike",101)])

instance Functor DB where

instance Applicative DB where

instance Monad DB where
    -- "bind"
    (>>=) = splice
    return = Return

p1'' :: DB String
p1'' = do put "Mike" 100
          x <- get "Mike"
          put "Mike" (x+1)
          y <- get "Mike"
          return (show (x+y))

-- >>> :info Monad
-- type Monad :: (* -> *) -> Constraint
-- class Applicative m => Monad m where
--   (>>=) :: m a -> (a -> m b) -> m b
--   (>>) :: m a -> m b -> m b
--   return :: a -> m a
--   {-# MINIMAL (>>=) #-}
--   	-- Defined in ‘GHC.Base’
-- instance Monad (Either e) -- Defined in ‘Data.Either’
-- instance Monad [] -- Defined in ‘GHC.Base’
-- instance Monad Solo -- Defined in ‘GHC.Base’
-- instance Monad Maybe -- Defined in ‘GHC.Base’
-- instance Monad IO -- Defined in ‘GHC.Base’
-- instance Monad ((->) r) -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b, Monoid c) => Monad ((,,,) a b c)
--   -- Defined in ‘GHC.Base’
-- instance (Monoid a, Monoid b) => Monad ((,,) a b)
--   -- Defined in ‘GHC.Base’
-- instance Monoid a => Monad ((,) a) -- Defined in ‘GHC.Base’


runDB :: DB a -> Map Key Value -> (a, Map Key Value)

-- >>> runDB p1 Map.empty
-- ("201",fromList [("Mike",101)])

runDB (Get key callback) mp = 
    let value = mp ! key
    in runDB (callback value) mp
runDB (Put key value callback) mp = 
    let mp' = Map.insert key value mp
    in runDB (callback ()) mp'
runDB (Return result) mp = (result, mp)


-- fmap ::        (a ->   b) -> f a -> f b
-- (<*>) ::    f  (a ->   b) -> f a -> f b
-- flip (>>=) ::  (a -> f b) -> f a -> f b