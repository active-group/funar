module Effect where

import System.IO

-- Datei kopieren
main :: IO ()
main = 
    do fromHandle <- getAndOpenFile "Copy from: " ReadMode
       toHandle   <- getAndOpenFile "Copy to: "   WriteMode
       contents <- hGetContents fromHandle
       hPutStr toHandle contents
       hClose fromHandle
       hClose toHandle
       
-- Dateinamen vom Benutzer abfragen und Datei öffnen
getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile prompt mode =
    do putStr prompt
       name <- getLine -- getLine :: IO String, name :: String
       openFile name mode
