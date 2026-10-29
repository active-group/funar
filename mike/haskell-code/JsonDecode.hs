module JsonDecode where

import qualified Data.Map.Strict as Map
import Data.Map (Map)

data Json =
    Number Double
  | String String
  | Bool Bool
  | Null
  | Array [Json]
  | Object (Map String Json)
  deriving Show

