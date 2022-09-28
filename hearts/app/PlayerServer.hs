{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module PlayerServer (runServer) where

import Cards
import Control.Monad.IO.Class (liftIO)
import qualified Data.IORef as IORef
import GameEvent
import HeartsJson ()
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Polysemy
import Polysemy.State (State)
import qualified Polysemy.State as State
import Servant

import Player
import PlayerIO

type PlayerAPI = "event" :> ReqBody '[JSON] GameEvent :> Post '[JSON] [GameCommand]

playerAPI :: Proxy PlayerAPI
playerAPI = Proxy

mkApp :: (GameEvent -> IO [GameCommand]) -> Application
mkApp eventProcessor =
  cors (const $ Just policy) $
    provideOptions playerAPI $
      serve playerAPI (liftIO . eventProcessor)
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}

runServer :: Int -> Maybe String -> Maybe String -> IO ()
runServer port (Just id) (Just name) =
  let player = Player id name
   in do
        putStrLn ("Starting player server for player '" ++ name ++ "' with ID '" ++ id ++ "' on port " ++ show port)
        eventProcessor <- playerIO player (alongStrategy @"Generic" :: Strategy '[State (PlayerState "Generic")])
        Network.Wai.Handler.Warp.run port (mkApp eventProcessor)
runServer _ _ _ = error "Missing player credentials"
