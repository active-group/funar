module Free where

{-
data DB a
  = Get Key (Value -> DB a) -- Callback / Continuation
  | Put Key Value (() -> DB a)
  | Return a


-}