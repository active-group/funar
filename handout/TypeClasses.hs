-- Typklassen

class Semigroup a where
  (<>) :: a -> a -> a

-- x <> (y <> z) = (x <> y) <> z

class Semigroup a => Monoid a where
  mempty  :: a
  mappend :: a -> a -> a
  mappend = (<>)

{-
x <> mempty = x
mempty <> x = x
x <> (y <> z) = (x <> y) <> z
-}

class Functor f where
  fmap :: (a -> b) -> f a -> f b

{-
fmap id = id
fmap (h . g) = fmap h . fmap g
-}

class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

{-
pure id <*> v = v
pure f <*> pure x = pure (f x)
u <*> pure y = pure ($ y) <*> u
pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-}

class Applicative m => Monad m where
    (>>=)       :: forall a b. m a -> (a -> m b) -> m b

    return      :: a -> m a
    return      = pure

{-
return a >>= k  =  k a
m >>= return =  m
m >>= (\\x -> k x >>= h)  =  (m >>= k) >>= h
-}
