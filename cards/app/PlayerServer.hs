{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module PlayerServer (runServer) where

import           Control.Monad.Identity
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.State
import           Data.Aeson                             (FromJSON, FromJSONKey,
                                                         ToJSON, ToJSONKey)
import qualified Data.IORef                             as IORef
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Servant.Options
import           Servant
import           Servant.API

import           Game
import           Orphans
import           Player

type PlayerAPI = "event" :> ReqBody '[JSON] GameEvent :> Post '[JSON] [GameCommand]

server :: EventProcessor IO GameEvent GameCommand -> Server PlayerAPI
server = (.) liftIO

playerAPI :: Proxy PlayerAPI
playerAPI = Proxy

mkApp :: EventProcessor IO GameEvent GameCommand -> Application
mkApp eventProcessor =
  cors (const $ Just policy)
  $ provideOptions playerAPI
  $ serve playerAPI (server eventProcessor)
    where
      policy = simpleCorsResourcePolicy { corsRequestHeaders = [ "content-type" ] }

ioEventProcessor :: Player -> Strategy (StateT PlayerState) -> IO (EventProcessor IO GameEvent GameCommand)
ioEventProcessor player (Strategy strategy) =
  do ref <- IORef.newIORef emptyPlayerState
     let processEvent event =
           do playerState <- IORef.readIORef ref
              (gameCommands, playerState') <- runStateT (strategy player event) playerState
              IORef.writeIORef ref playerState'
              return gameCommands
     return processEvent

runServer :: Int -> Maybe String -> Maybe String -> IO ()
runServer port (Just id) (Just name) =
  let player = Player id name
  in do
    putStrLn ("Starting player server for player '" ++ name ++ "' with ID '" ++ id ++ "' on port " ++ show port)
    eventProcessor <- ioEventProcessor player alongStrategy
    run port (mkApp eventProcessor)
runServer _ _ _ = error "Missing player credentials"
