{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
module Infinity where
import Prelude hiding ((==),(<),(>),(<=),(>=),(^),(<>))
import Language.BASIC

main :: IO ()
main = runBASIC $ do

    10 LET I := 1
    20 LET S := 0
    30 LET S := S + 1/I
    40 LET I := I + 1
    50 IF I <> 100000000 THEN 30
    60 PRINT "Almost infinity is ";S
    80 END
