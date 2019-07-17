module Effect where

-- IO a: Beschreibung von Effekten, die Ergebnis vom Typ a produzieren
-- ghci führt diese dann automatisch aus
-- oder:
-- main :: IO ()
import System.IO

main = do srcHandle <- getAndOpenFile "Quelldatei:" ReadMode
          dstHandle <- getAndOpenFile "Zieldatei:" WriteMode
          contents <- hGetContents srcHandle
          hPutStr dstHandle contents
          hClose srcHandle
          hClose dstHandle
          putStrLn "Fertig!"
          

-- Dateinamen einlesen und Datei öffnen
getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile prompt mode =
    do putStrLn prompt
       filename <- getLine
       openFile filename mode
  