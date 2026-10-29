{-# LANGUAGE InstanceSigs #-}
module JsonDecode where

import qualified Data.Map.Strict as Map
import Data.Map (Map)

import GHC.Float.RealFracMethods (truncateDoubleInteger)

data Json =
    Number Double
  | String String
  | Bool Bool
  | Null
  | Array [Json]
  | Object (Map String Json)
  deriving Show

data Error =
    Failure String Json
  | InField String Error
  | InArray Integer Error
  deriving Show
  
data Decoder a = Decoder (Json -> Either Error a)

instance Functor Decoder where
    fmap :: (a -> b) -> Decoder a -> Decoder b
    fmap f (Decoder decode) =
        Decoder (\json ->
         case decode json of
            Left error -> Left error
            Right a -> Right (f a))

instance Applicative Decoder where
    pure :: a -> Decoder a
    pure a = Decoder (\_ -> Right a)
    (<*>) :: Decoder (a -> b) -> Decoder a -> Decoder b
    (<*>) (Decoder decodeF) (Decoder decodeA) =
        Decoder ((<*>) <$> decodeF <*> decodeA)

runDecoder :: Decoder a -> Json -> Either Error a
runDecoder (Decoder decode) json = decode json

instance Monad Decoder where
    (>>=) :: Decoder a -> (a -> Decoder b) -> Decoder b
    (>>=) (Decoder decode) next =
        Decoder ((>>=) <$> decode <*> flip (runDecoder . next))

string :: Decoder String

-- >>> runDecoder string (String "hello world")
-- Right "hello world"
-- >>> runDecoder string (Number 42)
-- Left (Failure "string expected" (Number 42.0))

string = Decoder (\json ->
            case json of
                String s -> Right s
                _ -> Left (Failure "string expected" json))

integer :: Decoder Integer
integer = Decoder (\json ->
            case json of
                Number n -> Right (truncateDoubleInteger n)
                _ -> Left (Failure "number expected" json))

bool :: Decoder Bool
bool = undefined

list :: Decoder a -> Decoder [a]

-- >>> runDecoder (list string) (Array [String "foo", String "bar", String "baz"])
-- Right ["foo","bar","baz"]
-- >>> runDecoder (list string) (Array [String "foo", Number 42, String "baz"])
-- Left (InArray 1 (Failure "string expected" (Number 42.0)))


list (Decoder decodeElement) = Decoder (\json ->
    case json of
        Array elements ->
            traverse (\(index, element) ->
                       mapLeft (InArray index) (decodeElement element))
              (zip [0..] elements)
        _ -> Left (Failure "array expected" json))

mapLeft :: (a -> h) -> Either a b -> Either h b
mapLeft f (Left a) = Left (f a)
mapLeft f (Right b) = Right b
