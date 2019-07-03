module Effect where

import System.IO

getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile prompt mode =
    do putStr prompt
       name <- getLine -- :: IO String
       handle <- openFile name mode
       return handle

main :: IO ()
main = do fromHandle <- getAndOpenFile "Source file:" ReadMode
          toHandle <- getAndOpenFile "Dest file:" WriteMode
          contents <- hGetContents fromHandle
          hPutStr toHandle contents
          hClose toHandle
          putStr "Done!"