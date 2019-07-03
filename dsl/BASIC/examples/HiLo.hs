{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
module HiLo where
import Language.BASIC
import Prelude hiding ((==),(<),(>),(<=),(>=),(^),(<>))

main :: IO ()
main = runBASIC $ do
    10 GOSUB 1000
    20 PRINT "* Welcome to HiLo *"
    30 GOSUB 1000

    100 LET I := INT(100 * RND(X))
--    110 PRINT I
    200 PRINT "Guess my number:"
    210 INPUT X
    220 LET S := SGN(I-X)
    230 IF S <> 0 THEN 300

    240 FOR X := 1 TO 5
    250   PRINT X*X;" You won!"
    260 NEXT X
    270 STOP

    300 IF S <> 1 THEN 400
    310 PRINT "Your guess ";X;" is too low."
    320 GOTO 200

    400 PRINT "Your guess ";X;" is too high."
    410 GOTO 200

    1000 PRINT "*******************"
    1010 RETURN

    9999 END
