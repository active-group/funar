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
import Player
import Polysemy
import Polysemy.State (State)
import qualified Polysemy.State as State
import Servant

type PlayerAPI = "event" :> ReqBody '[JSON] GameEvent :> Post '[JSON] [GameCommand]

server :: EventProcessor '[Embed IO] GameEvent GameCommand -> Server PlayerAPI
server eventProcessor = \event -> liftIO (runM (eventProcessor event))

playerAPI :: Proxy PlayerAPI
playerAPI = Proxy

mkApp :: EventProcessor '[Embed IO] GameEvent GameCommand -> Application
mkApp eventProcessor =
  cors (const $ Just policy) $
    provideOptions playerAPI $
      serve playerAPI (server eventProcessor)
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}

ioEventProcessor ::
  Player ->
  Strategy '[State (PlayerState "Generic")] ->
  IO (EventProcessor '[Embed IO] GameEvent GameCommand)
ioEventProcessor player strategy =
  do
    ref <- IORef.newIORef emptyPlayerState
    let processEvent event =
          State.runStateIORef ref (raiseUnder @(Embed IO) (strategy player event))
    return processEvent

runServer :: Int -> Maybe String -> Maybe String -> IO ()
runServer port (Just id) (Just name) =
  let player = Player id name
   in do
        putStrLn ("Starting player server for player '" ++ name ++ "' with ID '" ++ id ++ "' on port " ++ show port)
        eventProcessor <- ioEventProcessor player (alongStrategy @"Generic")
        Network.Wai.Handler.Warp.run port (mkApp eventProcessor)
runServer _ _ _ = error "Missing player credentials"
