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
import Servant

import Player

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

runServer :: Int -> Maybe String -> IO ()
runServer port (Just name) =
  let player = Player name
   in do
        putStrLn ("Starting player server for player '" ++ name ++ "'  on port " ++ show port)
        -- eventProcessor :: GameEvent -> IO (GameEvent -> IO [GameCommand])
        eventProcessor <- statePlayerIO player (alongStrategy player)
        Network.Wai.Handler.Warp.run port (mkApp eventProcessor)
runServer _ _ = error "Missing player credentials"
