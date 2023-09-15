module TableIO where

import Data.IORef (IORef)
import qualified Data.IORef as IORef

import Cards
import GameEvent
import Table

tableIO :: [Player] -> IO (GameCommand -> IO [GameEvent])
tableIO players =
  do ref <- IORef.newIORef (tableLoopM, emptyTableState players)
     let processCommand command =          
           do (next, state) <- IORef.readIORef ref
              putStrLn ("processCommand " ++ (show command))
              let (state', events, step) = runTable (next command) state []              
              case step of
                Left cont ->
                  do putStrLn ("NeedsCommand")
                     IORef.writeIORef ref (cont, state')
                Right _result -> return ()
              return events
     return processCommand
