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
  deriving (Show, Eq)

{-
data Either a b = Left a | Right b
-}

newtype Decoder a = Decoder {runDecoder :: Json.Value -> Either DecodeError a}

instance Functor Decoder where
  -- fmap :: (a -> b) -> Decoder a -> Decoder b
--  fmap f (Decoder decode) = Decoder (\ json ->
--     case decode json of
--       Left error -> Left error
--       Right result -> Right (f result)
--    ) 
   fmap f (Decoder decode) = Decoder (\ json ->
     fmap f (decode json))

instance Applicative Decoder where
  -- pure :: a -> Decoder a
  pure a = Decoder (\ json -> Right a)
  -- (<*>) :: Decoder (a -> b) -> Decoder a -> Decoder b
  Decoder decodeF <*> Decoder decodeA =
    Decoder (\ json ->
      case decodeF json of
        Left error -> Left error
        Right f ->
          case decodeA json of
            Left error -> Left error
            Right a -> Right (f a))

instance Monad Decoder where
  -- flip fmap :: Decoder a -> (a ->         b) -> Decoder b
  -- (>>=) ::     Decoder a -> (a -> Decoder b) -> Decoder b
  Decoder decode >>= next = 
    Decoder (\ json ->
      case decode json of
        Left error -> Left error
        Right a ->
          -- nextDecode :: Json.Value -> Either DecodeError b
          let Decoder nextDecode = next a
          in nextDecode json)



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
