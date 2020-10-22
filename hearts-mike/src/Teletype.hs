{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
module Teletype where

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Internal (send)

-- GADTs: Generalized Abstract Datatypes
data Teletype m result where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

readTTY :: Member Teletype r => Sem r String
-- readTTY :: Sem [Teletype] String
readTTY = send ReadTTY

writeTTY :: Member Teletype effects => String -> Sem effects ()
writeTTY string = send (WriteTTY string)

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

runTeletype' :: Sem (Teletype ': r) a -> Sem (Embed IO ': r) a
runTeletype' = reinterpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

runTeletype :: Sem '[Teletype] a -> IO a
runTeletype = runM . runTeletype'

runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i
  = runOutputMonoid pure  -- For each WriteTTY in our program, consume an output by appending it to the list in a ([String], a)
  . runInputList i         -- Treat each element of our list of strings as a line of input
  . reinterpret2 \case     -- Reinterpret our effect in terms of Input and Output
      ReadTTY -> maybe "" id <$> input
      WriteTTY msg -> output msg


echo :: Member Teletype r => Sem r ()
echo = do
  input <- readTTY
  case input of
    "" -> return ()
    _  -> do writeTTY input
             echo


-- Let's pretend
echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTeletypePure echo

