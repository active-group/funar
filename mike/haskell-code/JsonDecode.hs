{-# LANGUAGE OverloadedStrings #-}
module JsonDecode where

import qualified Data.Aeson as Json
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Either as Either
import Data.Either.Combinators (mapLeft)
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import Data.String (fromString)

data DecodeError
  = Field String DecodeError
  | Index Int DecodeError
  | OneOf [DecodeError]
  | Failure String Json.Value
  deriving (Show, Eq)

error1 = Failure "expected string, but got number" (Json.Number 5)
-- { "foo": 5 }
error2 = Field "foo" error1

-- >>> Json.decode "{\"foo\": 123}" :: Maybe Json.Value
-- Just (Object (fromList [("foo",Number 123.0)]))

-- data Either left right = Left left | Right right
newtype Decoder a = Decoder {runDecoder :: Json.Value -> Either DecodeError a}

instance Functor Decoder where
  fmap f (Decoder decode) = Decoder (\ json ->
    fmap f (decode json))

instance Applicative Decoder where
  pure = Decoder . const . Right
  Decoder decodeF <*> Decoder decodeA = Decoder (\ json ->
    case decodeA json of
      Right a ->
        case decodeF json of
          Right f -> Right (f a)
          Left err -> Left err
      Left err -> Left err)

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

faild :: String -> Decoder a
faild message = Decoder (\ json ->
  Left (Failure message json))

list :: Decoder a -> Decoder [a]
list (Decoder decodeElement) =
  Decoder (\json ->
    case json of
      Json.Array arr -> traverse decodeElement (Vector.toList arr)
      _ -> Left (Failure "not a json array" json)
    )

data Example = Example { exampleFoo :: Int, exampleBaz :: String}
-- { "foo" : 15, "bar": "baz" }

decodeExample = Example <$> (field "foo" int) <*> (field "bar" string)

decodeExample' json =
  let fooDecoder = field "foo" int
      barDecoder = field "bar" string
      fooResult = runDecoder fooDecoder json
      barResult = runDecoder barDecoder json
  in
    case (fooResult, barResult) of
      (Right fooValue, Right barValue) ->
        Right (Example fooValue barValue)
      (Left error, _) -> Left error
      (_, Left error) -> Left error

field :: String -> Decoder a -> Decoder a
field name (Decoder decodeField) =
  Decoder (\json ->
    case json of
      Json.Object fields ->
        case KeyMap.lookup (fromString name) fields of
          Just val ->
            mapLeft (Field name) (decodeField val)
          Nothing -> Left (Failure ("Field " ++ name ++ " not found") json)
      _ -> Left (Failure "not a JSON object" json))

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


