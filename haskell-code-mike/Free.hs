module Free where

-- f: Datentyp mit den Effekten
data Free f a =
    -- Done :: a -> Free f a
    Done a
  | Effectful (f (Free f a))

instance Functor (Free f) where

instance Applicative (Free f) where

instance Functor f => Monad (Free f) where
    return = Done

    (Done result) >>= next = next result
    (Effectful f) >>= next =
        Effectful (fmap (>>= next) f)


data DB' knot =
   Get String (Integer -> knot)
 | Put String Integer (() -> knot)



type DB a = Free DB' a