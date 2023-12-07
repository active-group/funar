module Free where 

{-
data DB a
  = Get Key (Value -> DB a)
  | Put Key Value (() -> DB a)
                         ^^^^
  | Return a
    ^^^^^^^^
->

data DB' self =
  = Get Key (Value -> self)
  | Put Key Value (() -> self)


-}
data Free f a =
     Pure a
   | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure r) = Impure (fmap (fmap f) r)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (Pure f) <*> aa = fmap f aa
  (Impure fa) <*> aa = Impure (fmap (<*> aa) fa)

instance Functor f => Monad (Free f) where
    return = Pure
    
    (Pure a) >>= k = k a
    (Impure r) >>= k = Impure (fmap (>>= k) r)

runFree :: Monad m => (f (Free f a) -> (Free f a -> m a) -> m a) -> Free f a -> m a
runFree _ (Pure result) = return result
runFree rrr (Impure command) = rrr command (runFree rrr)
