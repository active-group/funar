module Free where 

data Free f a =
     Return a
   | Impure (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap f (Return a) = Return (f a)
  fmap f (Impure r) = Impure (fmap (fmap f) r)

instance Functor f => Applicative (Free f) where
  pure = Return
  (Return f) <*> aa = fmap f aa
  (Impure fa) <*> aa = Impure (fmap (<*> aa) fa)

instance Functor f => Monad (Free f) where
    return = Return
    
    (Return a) >>= k = k a
    (Impure r) >>= k = Impure (fmap (>>= k) r)

runFree :: Monad m => (f (Free f a) -> (Free f a -> m a) -> m a) -> Free f a -> m a
runFree _ (Return result) = return result
runFree rrr (Impure command) = rrr command (runFree rrr)
