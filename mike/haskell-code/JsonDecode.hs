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

newtype Decoder a = Decoder {runDecoder :: Json.Value -> Either DecodeError a}

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

