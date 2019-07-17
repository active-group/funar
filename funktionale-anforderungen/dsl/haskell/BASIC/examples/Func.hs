{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
module Func where
import Language.BASIC

main :: IO ()
main = runBASIC $ do

    10 FOR X := 0 TO 2.01 STEP 0.1
    30   PRINT SIN(X);" ";COS(X);" ";TAN(X);" ";ATN(X);" ";EXP(X);" ";LOG(X);" ";SQR(X);" ";ABS(X);" ";SGN(X);" ";INT(X);" ";RND(X);" ";X
    40 NEXT X
    100 END
