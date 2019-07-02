{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
module Hello where
import Language.BASIC

main :: IO ()
main = runBASIC $ do

    10 PRINT "Hello BASIC World!"
