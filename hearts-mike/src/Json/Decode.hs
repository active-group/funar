module Json.Decode where

import qualified Data.Aeson as Json
import qualified Data.Either as Either
import Data.Either.Combinators (mapLeft)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Vector as Vector

data DecodeError
  = Field String DecodeError
  | Index Int DecodeError
  | OneOf [DecodeError]
  | Failure String Json.Value
  | Combined DecodeError DecodeError
  deriving (Show, Eq)

-- data Either c a = Left c | Right a

newtype Decoder a = Decoder {runDecoder :: Json.Value -> Either DecodeError a}


{-
instance Functor (Either b) where
  -- fmap :: (a -> b) -> (Either c) a -> (Either c) b
  fmap f (Left c) = Left c
  fmap f (Right a) = Right (f a)
-}

instance Functor Decoder where
  fmap f (Decoder decode) = Decoder (\ json ->
    -- fmap von Either b
    fmap f (decode json))

-- >>> :type (>>=)
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- >>> :type (flip (>>=))
-- (flip (>>=)) :: Monad m => (a -> m b) -> m a -> m b
{-
Functor
fmap  ::        (a ->   b) -> f a -> f b
Applicative
(<*>) ::      f (a ->   b) -> f a -> f b
Monad
(flip (>>=)) :: (a -> f b) -> f a -> f b
-}

instance Applicative Decoder where
  -- das gleiche wie return
  pure = Decoder . const . Right
  Decoder decodeF <*> Decoder decodeA = 
    Decoder (\json ->
      case (decodeF json, decodeA json) of
        (Left errF, Left errA) -> Left (Combined errF errA)
        (Left errF, Right a) -> Left errF
        (Right f, Left errA) -> Left errA
        (Right f, Right a) -> Right (f a))

-- optionalMap  ::              (a -> b) -> Optional a -> Optional b
-- universalMap :: Functor f => (a -> b) ->        f a ->        f b

-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
fmap2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- >>> fmap2 (+) (Just 2) (Just 3)
-- Just 5
-- >>> fmap2 (+) [3,4] [5,6]
-- [8,9,9,10]
fmap2 f fa fb = -- für <*> brauchen wir f (a -> b)
  -- pure f -- f (a -> (b -> c))
  (pure f) <*> fa <*> fb

fmap3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
fmap3 f fa fb fc =
  f <$> fa <*> fb <*> fc

instance Monad Decoder where
  Decoder decode >>= f = Decoder (\ json ->
    decode json >>= (\a -> runDecoder (f a) json))

string :: Decoder String
string = Decoder (\ json ->
  case json of
    Json.String s -> Right (Text.unpack s)
    json -> Left (Failure "Not a string" json))

int :: Decoder Int
int = Decoder (\ json ->
  let failure = Left (Failure "Not an int" json)
   in case json of
        Json.Number s -> maybe failure Right (Scientific.toBoundedInteger s)
        _ -> failure)

double :: Decoder Double
double = Decoder (\ json ->
  case json of
    Json.Number s -> Right (Scientific.toRealFloat s)
    json -> Left (Failure "Not an int" json))

bool :: Decoder Bool
bool = Decoder (\ json ->
  case json of
    Json.Bool b -> Right b
    json -> Left (Failure "Not a boolean" json))

jsonNull :: a -> Decoder a
jsonNull val = Decoder (\ json ->
  case json of
    Json.Null -> Right val
    json -> Left (Failure "Not null" json))

list :: Decoder a -> Decoder [a]
list (Decoder decodeElement) = Decoder (\ json ->
  case json of
  Json.Array arr -> traverse decodeElement (Vector.toList arr)
  json -> Left (Failure "Not a JSON array" json))

index :: Int -> Decoder a -> Decoder a
index n (Decoder decodeElement) = Decoder (\ json ->
  case json of
    Json.Array arr ->
      if n < length arr
        then mapLeft (Index n) (decodeElement (arr Vector.! n))
        else Left (Failure ("Index out of bounds: " ++ show n) json)
    _ -> Left (Failure "Not a JSON array" json))

oneOf :: [Decoder a] -> Decoder a
oneOf decoders = Decoder (\ json ->
  let results = map (\(Decoder f) -> f json) decoders
   in case Either.rights results of
        [] -> Left (OneOf (Either.lefts results))
        (x : _) -> Right x)

optional :: Decoder a -> Decoder (Maybe a)
optional decoder =
  oneOf [fmap Just decoder, pure Nothing]

field :: String -> Decoder a -> Decoder a
field name decoder = Decoder (\json ->
  case json of
    Json.Object fields ->
      case HashMap.lookup (Text.pack name) fields of
        Just val -> mapLeft (Field name) (runDecoder decoder val)
        Nothing -> Left (Failure ("Field " ++ name ++ " not found") json)
    _ -> Left (Failure "Not a JSON object" json))

faild :: String -> Decoder a
faild message = Decoder (\ json ->
  Left (Failure message json))
