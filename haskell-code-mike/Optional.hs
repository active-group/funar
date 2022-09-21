module Optional where

data Optional a
  = Null
  | Result a
  deriving (Show)

instance Functor Optional where

instance Applicative Optional where

instance Monad Optional where
    -- Euer Code hier!