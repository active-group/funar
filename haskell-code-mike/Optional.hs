module Optional where

data Optional a
  = Null
  | Result a
  deriving (Show)

instance Functor Optional where

instance Applicative Optional where

instance Monad Optional where
    -- Euer Code hier!
    return = Result
    (>>=) Null next = Null
    (>>=) (Result a) next = next a

-- >>> map2 (+) (Result 5) (Result 7)
-- Result 12
-- >>> map2 (+) Null (Result 7)
-- Null
map2 :: (a -> b -> c) -> Optional a -> Optional b -> Optional c
map2 f opt1 opt2 =
   do val1 <- opt1
      val2 <- opt2
      return (f val1 val2)

map3 :: (a -> b -> c -> d) -> Optional a -> Optional b -> Optional c -> Optional d
map3 f opt1 opt2 opt3 =
  do
    val1 <- opt1
    val2 <- opt2
    val3 <- opt3
    return (f val1 val2 val3)
