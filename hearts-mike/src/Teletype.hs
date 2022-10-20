{-# LANGUAGE LambdaCase, BlockArguments, ScopedTypeVariables #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}
module Teletype where

import Polysemy
import Polysemy.Internal (send)
import Polysemy.Input
import Polysemy.Output

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

-- In Polysemy gibt es *eine* Monade Sem mit einem Typparameter,
-- eine Liste von Effekten

-- Sem effects a: "effektbehaftete Berechnung" mit Effekten effects und 
-- Resultat a

-- Annahme: Sem '[DB Game] a
--        + Sem '[Game] a

readTTY :: Member Teletype effects => Sem effects String 
readTTY = send ReadTTY
writeTTY :: Member Teletype effects => String -> Sem effects ()
writeTTY text = send (WriteTTY text)

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
  i <- readTTY
  case i of
    "" -> return ()
    _  -> writeTTY i >> echo


-- Let's pretend
echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTeletypePure echo

