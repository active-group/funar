{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module TableServer (runServer) where

import Cards
import Control.Monad.IO.Class (liftIO)
import GameEvent
import HeartsJson ()
import Network.Wai
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options
import Servant
import TableIO

type TableAPI = "command" :> ReqBody '[JSON] GameCommand :> Post '[JSON] [GameEvent]

server :: (GameCommand -> IO [GameEvent]) -> Server TableAPI
server = (.) liftIO

tableAPI :: Proxy TableAPI
tableAPI = Proxy

mkApp :: (GameCommand -> IO [GameEvent]) -> Application
mkApp commandProcessor =
  cors (const $ Just policy) $
    provideOptions tableAPI $
      serve tableAPI (server commandProcessor)
  where
    policy = simpleCorsResourcePolicy {corsRequestHeaders = ["content-type"]}

runServer :: Int -> IO ()
runServer port = do
  putStrLn ("Table server running on port " ++ show port)
  commandProcessor <- tableIO [Player "Mike", Player "Peter", Player "Nicole", Player "Annette"]
  run port (mkApp commandProcessor)
