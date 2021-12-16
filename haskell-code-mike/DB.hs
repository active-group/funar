module DB where

import Data.Map

{-
put "Mike" 50
x = get "Mike"
put "Mike" (x + 1)
y = get "Mike"
return (x + y)
-}
{-
data DBCommand =
    Put String Integer
  | Get String 

type DBProgram = [DBCommand]

p1 :: [DBCommand]
p1 = [Put "Mike" 50, Get "Mike" ] -- können Ergebnis keinen Namen geben: fail
-}

-- Operationen:
-- - write
-- - look
data State state a =
    Write state (() -> State state a)
  | Look (state -> State state a)
  | Return' a

write :: state -> State state ()
write newState = undefined
look :: State state state
look = undefined

data DB a =
    Get String         (Integer -> DB a) -- continuation
  | Put String Integer (()      -> DB a)
  | Return a

get :: String -> DB Integer
-- get key = Get key (\ value -> Return value)
get key = Get key Return

put :: String -> Integer -> DB ()
put key value = Put key value Return

c1 :: DB ()
c1 = put "Mike" 50
c2 :: DB Integer
c2 = get "Mike"

splice :: DB a -> (a -> DB b) -> DB b
splice (Get key cont) next = 
  Get key (\ value ->
      splice (cont value) next)
splice (Put key value cont) next = 
  Put key value (\ () ->
      splice (cont ()) next) 
splice (Return result) next = next result 

p1' :: DB Integer 
p1' = put "Mike" 50 `splice` (\() ->
      get "Mike" `splice` (\ x ->
      put "Mike" (x + 1) `splice` (\ () ->
      get "Mike" `splice` (\ y ->
      Return (x + y)))))
    
p1 :: DB Integer
p1 = Put "Mike" 50 (\() ->
     Get "Mike" (\ x ->
     Put "Mike" (x + 1) (\ () ->
     Get "Mike" (\ y ->
     Return (x + y)))))

instance Functor DB where 

instance Applicative DB where

instance Monad DB where
    (>>=) = splice -- "bind", "flatMap"
    return = Return

p1'' = do put "Mike" 50
          x <- get "Mike"
          put "Mike" (x + 1)
          y <- get "Mike"
          return ( x+ y)

-- Interpreter
runDB :: DB a -> Map String Integer -> a
runDB (Get key cont) mp = 
    runDB (cont (mp ! key)) mp
runDB (Put key value cont) mp = 
    runDB (cont ()) (insert key value mp)
runDB (Return result) mp = result